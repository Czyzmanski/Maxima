#ifndef FUNCTION_MAXIMA_H
#define FUNCTION_MAXIMA_H

#include <set>
#include <map>
#include <memory>

namespace {
    template<typename T>
    bool operator==(const T &t_1, const T &t_2) {
        return !(t_1 < t_2) && !(t_2 < t_1);
    }
}


class InvalidArg : public std::exception {
public:
    InvalidArg() noexcept = default;

    InvalidArg(const InvalidArg &other) noexcept = default;

    InvalidArg(InvalidArg &&other) noexcept = default;

    InvalidArg &operator=(const InvalidArg &other) noexcept = default;

    InvalidArg &operator=(InvalidArg &&other) noexcept = default;

    [[nodiscard]] const char *what() const noexcept override {
        return "invalid argument value";
    }
};


template<typename A, typename V>
class FunctionMaxima {
public:
    class point_type {
        friend class FunctionMaxima;


    public:
        ~point_type() = default;

        point_type(const point_type &other) noexcept = default;

        point_type(point_type &&other) noexcept = default;

        point_type &operator=(const point_type &other) noexcept = default;

        point_type &operator=(point_type &&other) noexcept = default;

        const A &arg() const {
            return *a_ptr;
        }

        const V &value() const {
            return *v_ptr;
        }

    private:
        using a_ptr_t = std::shared_ptr<const A>;
        using v_ptr_t = std::shared_ptr<const V>;

        a_ptr_t a_ptr;
        v_ptr_t v_ptr;

        point_type() noexcept = default;

        explicit point_type(const A &a) : a_ptr{std::make_shared<const A>(a)} {}

        point_type(const A &a, const V &v) : a_ptr{std::make_shared<const A>(a)},
                                             v_ptr{std::make_shared<const V>(v)} {}
    };


    FunctionMaxima() noexcept = default;

    ~FunctionMaxima() = default;

    FunctionMaxima(const FunctionMaxima<A, V> &other) = default;

    FunctionMaxima(FunctionMaxima<A, V> &&other) noexcept = default;

    FunctionMaxima &operator=(const FunctionMaxima<A, V> &other) = default;

    FunctionMaxima &operator=(FunctionMaxima<A, V> &&other) noexcept = default;

    const V &value_at(const A &a) const {
        iterator it = find(a);
        if (it == end()) {
            throw InvalidArg();
        }
        else {
            return it->value();
        }
    }

    void set_value(const A &a, const V &v);

    void erase(const A &a);

private:
    /* Compares two points first by their arguments, then by their values. */
    class ArgCmp {
    public:
        bool operator()(const point_type &pt_1, const point_type &pt_2) const {
            if (pt_1.arg() == pt_2.arg()) {
                return pt_1.value() < pt_2.value();
            }
            else {
                return pt_1.arg() < pt_2.arg();
            }
        }
    };


    /* Compares two points first by their values, then by their arguments. */
    class ValCmp {
    public:
        bool operator()(const point_type &pt_1, const point_type &pt_2) const {
            if (pt_1.value() == pt_2.value()) {
                return pt_1.arg() < pt_2.arg();
            }
            else {
                return pt_2.value() < pt_1.value();
            }
        }
    };


    /* Compares two shared pointers by values of the objects they point to. */
    class SharedPtrCmp {
    public:
        bool operator()(const typename point_type::a_ptr_t &a_ptr_1,
                        const typename point_type::a_ptr_t &a_ptr_2) const {
            return *a_ptr_1 < *a_ptr_2;
        }
    };


    using pts_set_t = std::set<point_type, ArgCmp>;
    using mx_set_t = std::set<point_type, ValCmp>;
    using pts_map_t = std::map<typename point_type::a_ptr_t, point_type, SharedPtrCmp>;

    pts_set_t pts_set;
    mx_set_t mx_set;
    pts_map_t pts_map;


    template<typename T>
    class InsertGuard {
    public:
        explicit InsertGuard(T *container) noexcept: container{container},
                                                     rollback{false} {}

        ~InsertGuard() {
            if (rollback) {
                container->erase(it);
            }
        }

        InsertGuard(const InsertGuard &other) = delete;

        InsertGuard &operator=(const InsertGuard &other) = delete;

        InsertGuard(InsertGuard &&other) = delete;

        InsertGuard &operator=(InsertGuard &&other) = delete;

        /* Returns iterator to newly inserted element. */
        typename T::iterator insert(const point_type &pt) {
            it = container->insert(pt).first;
            rollback = true;

            return it;
        }

        void drop_rollback() {
            rollback = false;
        }

    private:
        T *container;
        typename T::iterator it;
        bool rollback;
    };


    template<typename T>
    class MapInsertGuard {
    public:
        explicit MapInsertGuard(T *container) : container{container},
                                                rollback{false},
                                                was_present{false} {}

        ~MapInsertGuard() {
            if (rollback) {
                if (!was_present) {
                    container->erase(it);
                }
                else {
                    it->second = prev_pt;
                }
            }
        }

        MapInsertGuard(const MapInsertGuard &other) = delete;

        MapInsertGuard &operator=(const MapInsertGuard &other) = delete;

        MapInsertGuard(MapInsertGuard &&other) = delete;

        MapInsertGuard &operator=(MapInsertGuard &&other) = delete;

        /* Returns iterator to newly inserted element. */
        typename T::iterator insert(const point_type &pt) {
            auto old_it = container->find(pt.a_ptr);
            if (old_it != container->end()) {
                was_present = true;
                prev_pt = old_it->second;
            }
            it = container->insert_or_assign(pt.a_ptr, pt).first;
            rollback = true;

            return it;
        }

        void drop_rollback() {
            rollback = false;
        }

    private:
        T *container;
        typename T::iterator it;
        bool rollback;
        bool was_present;
        point_type prev_pt;
    };


    using iterator_t = typename pts_set_t::iterator;
    using mx_iterator_t = typename mx_set_t::iterator;

    /* Returns true if point pointed to by iterator m is local maximum
     * if point pointed to by iterator l is its left neighbor
     * and point pointed to by iterator r is its right neighbor.
     * If l == m or r == end(), then point pointed to by iterator m has no left
     * neighbor or has no right neighbor, respectively. */
    bool is_local_mx(iterator_t l, iterator_t m, iterator_t r) const {
        return (l == m || !(m->value() < l->value()))
               && (r == end() || !(m->value() < r->value()));
    };

    bool mx_set_contains(iterator_t it) const {
        return mx_set.find(*it) != mx_end();
    }

    using neighborhood_t = std::tuple<iterator_t, iterator_t, iterator_t>;

    neighborhood_t l_pt_neighborhood(iterator_t old_it, iterator_t new_it) {
        iterator_t m = std::prev(new_it) != old_it ?
                       std::prev(new_it) : std::prev(new_it, 2);
        iterator_t l = m != begin() ? std::prev(m) : m;
        iterator_t r = new_it;

        return {l, m, r};
    }

    neighborhood_t new_pt_neighborhood(bool has_l, iterator_t old_it,
                                       iterator_t new_it) {
        iterator l = has_l ? (std::prev(new_it) != old_it ?
                              std::prev(new_it) : std::prev(new_it, 2)) : new_it;
        iterator m = new_it;
        iterator r = std::next(new_it) != old_it ?
                     std::next(new_it) : std::next(new_it, 2);

        return {l, m, r};
    }

    neighborhood_t r_pt_neighborhood(iterator_t old_it, iterator_t new_it) {
        iterator_t l = new_it;
        iterator_t m = std::next(new_it) != old_it ?
                       std::next(new_it) : std::next(new_it, 2);
        iterator_t r = std::next(m);

        return {l, m, r};
    }

    mx_iterator_t update_l_pt_if_new_loc_mx(iterator_t old_it, iterator_t new_it,
                                            InsertGuard<mx_set_t> &insert_guard) {
        auto[l, m, r] = l_pt_neighborhood(old_it, new_it);
        mx_iterator_t mx_it = mx_set.find(*m);
        if (is_local_mx(l, m, r) && mx_it == mx_end()) {
            insert_guard.insert(*m);
        }

        return mx_it;
    }

    void update_l_pt_if_no_longer_loc_mx(mx_iterator_t mx_it,
                                         iterator_t old_it, iterator_t new_it,
                                         InsertGuard<mx_set_t> &insert_guard) {
        auto[l, m, r] = l_pt_neighborhood(old_it, new_it);
        if (!is_local_mx(l, m, r) && mx_it != mx_end()) {
            mx_set.erase(mx_it);
        }
    }

    void update_new_pt_if_new_loc_mx(bool has_l, iterator_t old_it,
                                     iterator_t new_it,
                                     InsertGuard<mx_set_t> &insert_guard) {
        auto[l, m, r] = new_pt_neighborhood(has_l, old_it, new_it);
        if (is_local_mx(l, m, r) && !mx_set_contains(m)) {
            insert_guard.insert(*m);
        }
    }

    mx_iterator_t update_r_pt_if_new_loc_mx(iterator_t old_it, iterator_t new_it,
                                            InsertGuard<mx_set_t> &insert_guard) {
        auto[l, m, r] = r_pt_neighborhood(old_it, new_it);
        mx_iterator_t mx_it = mx_set.find(*m);
        if (is_local_mx(l, m, r) && mx_it == mx_end()) {
            insert_guard.insert(*m);
        }

        return mx_it;
    }

    void update_r_pt_if_no_longer_loc_mx(mx_iterator_t mx_it,
                                         iterator_t old_it, iterator_t new_it,
                                         InsertGuard<mx_set_t> &insert_guard) {
        auto[l, m, r] = r_pt_neighborhood(old_it, new_it);
        if (!is_local_mx(l, m, r) && mx_it != mx_end()) {
            mx_set.erase(mx_it);
        }
    }

public:
    using iterator = typename pts_set_t::iterator;
    using mx_iterator = typename mx_set_t::iterator;
    using size_type = typename pts_set_t::size_type;

    iterator begin() const noexcept {
        return pts_set.begin();
    }

    iterator end() const noexcept {
        return pts_set.end();
    }

    iterator find(const A &a) const {
        auto map_it = pts_map.find(std::make_shared<const A>(a));
        if (map_it == pts_map.end()) {
            return end();
        }
        else {
            return pts_set.find(map_it->second);
        }
    }

    mx_iterator mx_begin() const noexcept {
        return mx_set.begin();
    }

    mx_iterator mx_end() const noexcept {
        return mx_set.end();
    }

    size_type size() const noexcept {
        return pts_set.size();
    }
};


template<typename A, typename V>
void FunctionMaxima<A, V>::set_value(const A &a, const V &v) {
    point_type pt{a, v};
    iterator old_it = pts_set.find(pt);
    if (old_it == end()) {
        InsertGuard<pts_set_t> pts_set_guard{&pts_set};
        MapInsertGuard<pts_map_t> pts_map_guard{&pts_map};
        InsertGuard<mx_set_t> l_mx_set_guard{&mx_set};
        InsertGuard<mx_set_t> new_mx_set_guard{&mx_set};
        InsertGuard<mx_set_t> r_mx_set_guard{&mx_set};

        iterator new_it = pts_set_guard.insert(pt);
        pts_map_guard.insert(pt);
        if (new_it != begin()) {
            iterator prev_it = std::prev(new_it);
            if (new_it->arg() == prev_it->arg()) {
                old_it = prev_it;
            }
        }
        if (std::next(new_it) != end()) {
            iterator next_it = std::next(new_it);
            if (new_it->arg() == next_it->arg()) {
                old_it = next_it;
            }
        }
        mx_iterator mx_iter = old_it == end() ? mx_end() : mx_set.find(*old_it);

        bool has_l = new_it != begin()
                     && (std::prev(new_it) != old_it || old_it != begin());
        bool has_r = std::next(new_it) != end()
                     && (std::next(new_it) != old_it || std::next(old_it) != end());

        mx_iterator mx_l_it, mx_r_it;
        if (has_l) {
            mx_l_it = update_l_pt_if_new_loc_mx(old_it, new_it, l_mx_set_guard);
        }
        update_new_pt_if_new_loc_mx(has_l, old_it, new_it, new_mx_set_guard);
        if (has_r) {
            mx_r_it = update_r_pt_if_new_loc_mx(old_it, new_it, r_mx_set_guard);
        }

        if (has_l) {
            update_l_pt_if_no_longer_loc_mx(mx_l_it, old_it, new_it, l_mx_set_guard);
        }
        if (has_r) {
            update_r_pt_if_no_longer_loc_mx(mx_r_it, old_it, new_it, r_mx_set_guard);
        }

        if (old_it != end()) {
            if (mx_iter != mx_end()) {
                mx_set.erase(mx_iter);
            }
            pts_set.erase(old_it);
        }

        pts_set_guard.drop_rollback();
        pts_map_guard.drop_rollback();
        l_mx_set_guard.drop_rollback();
        new_mx_set_guard.drop_rollback();
        r_mx_set_guard.drop_rollback();
    }
}

template<typename A, typename V>
void FunctionMaxima<A, V>::erase(const A &a) {
    iterator it = find(a);
    if (it != end()) {
        mx_iterator mx_it = mx_set.find(*it);
        auto map_it = pts_map.find(it->a_ptr);
        bool has_l = it != begin();
        bool has_r = std::next(it) != end();
        bool was_local_mx = mx_it != mx_end();
        InsertGuard<mx_set_t> l_guard{&mx_set}, r_guard{&mx_set};
        bool erase_left = false, erase_right = false;
        mx_iterator l_mx_it, r_mx_it;

        if (has_l) {
            iterator m = std::prev(it);
            iterator l = m != begin() ? std::prev(m) : m;
            iterator r = std::next(it);

            if (is_local_mx(l, m, r) && !mx_set_contains(m)) {
                l_guard.insert(*m);
            }
            else if (!is_local_mx(l, m, r) && mx_set_contains(m)) {
                erase_left = true;
                l_mx_it = mx_set.find(*m);
            }
        }
        if (has_r) {
            iterator l = has_l ? std::prev(it) : std::next(it);
            iterator m = std::next(it);
            iterator r = m != end() ? std::next(m) : m;

            if (is_local_mx(l, m, r) && !mx_set_contains(m)) {
                r_guard.insert(*m);
            }
            else if (!is_local_mx(l, m, r) && mx_set_contains(m)) {
                erase_right = true;
                r_mx_it = mx_set.find(*m);
            }
        }



        pts_set.erase(it);
        pts_map.erase(map_it);
        if (erase_left) {
            mx_set.erase(l_mx_it);
        }
        if (was_local_mx) {
            mx_set.erase(mx_it);
        }
        if (erase_right) {
            mx_set.erase(r_mx_it);
        }

        l_guard.drop_rollback();
        r_guard.drop_rollback();
    }
}

#endif // FUNCTION_MAXIMA_H
