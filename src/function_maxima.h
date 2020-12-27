#ifndef FUNCTION_MAXIMA_H
#define FUNCTION_MAXIMA_H

#include <set>
#include <memory>


class InvalidArg : public std::exception {
public:
    InvalidArg() noexcept = default;

    InvalidArg(const InvalidArg &other) noexcept = default;

    InvalidArg(InvalidArg &&other) noexcept = default;

    InvalidArg &operator=(const InvalidArg &other) noexcept = default;

    InvalidArg &operator=(InvalidArg &&other) noexcept = default;

    const char *what() const noexcept override {
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

        point_type(const point_type &other) = default;

        point_type(point_type &&other) noexcept = default;

        point_type &operator=(const point_type &other) = default;

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

        point_type() = default;

        explicit point_type(const A *a) : a_ptr(a) {}

        point_type(const A *a, const V &v) : a_ptr(a),
                                             v_ptr(std::make_shared<V>(v)) {}

        point_type(const A &a, const V &v) : a_ptr(std::make_shared<A>(a)),
                                             v_ptr(std::make_shared<V>(v)) {}
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
    class ArgCmp {
    public:
        bool operator()(const point_type &pt_1, const point_type &pt_2) const {
            if (!(pt_1.arg() < pt_2.arg()) && !(pt_2.arg() < pt_1.arg())) {
                return pt_1.value() < pt_2.value();
            }
            else {
                return pt_1.arg() < pt_2.arg();
            }
        }
    };


    class ValCmp {
    public:
        bool operator()(const point_type &pt_1, const point_type &pt_2) const {
            if (!(pt_1.value() < pt_2.value()) && !(pt_2.value() < pt_1.value())) {
                return pt_1.arg() < pt_2.arg();
            }
            else {
                return pt_1.value() < pt_2.value();
            }
        }
    };


    using pts_set_t = std::set<point_type, ArgCmp>;
    using max_set_t = std::set<point_type, ValCmp>;

    pts_set_t pts_set;
    max_set_t max_set;


    template<typename S>
    class InsertGuard {
    public:
        explicit InsertGuard(S *set) : set(set), rollback(false) {}

        ~InsertGuard() {
            if (rollback) {
                set->erase(it);
            }
        }

        InsertGuard(const InsertGuard &other) = delete;

        InsertGuard &operator=(const InsertGuard &other) = delete;

        InsertGuard(InsertGuard &&other) = delete;

        InsertGuard &operator=(InsertGuard &&other) = delete;

        typename S::iterator insert(const point_type &pt) {
            it = set->insert(pt).first;
            rollback = true;
            return it;
        }

        void drop_rollback() {
            rollback = false;
        }

    private:
        S *set;
        typename S::iterator it;
        bool rollback;
    };


    using iterator_t = typename pts_set_t::iterator;

    bool is_local_max(iterator_t left, iterator_t middle, iterator_t right) const {
        return (left == middle || !(middle->value() < left->value()))
               && (right == end() || !(middle->value() < right->value()));
    };

    bool mx_set_contains(iterator_t it) const {
        return max_set.find(*it) != mx_end();
    }

public:
    using iterator = typename pts_set_t::iterator;
    using mx_iterator = typename max_set_t::iterator;
    using size_type = typename pts_set_t::size_type;

    iterator begin() const noexcept {
        return pts_set.begin();
    }

    iterator end() const noexcept {
        return pts_set.end();
    }

    iterator find(const A &a) const {
        return pts_set.find(point_type(&a));
    }

    mx_iterator mx_begin() const noexcept {
        return max_set.begin();
    }

    mx_iterator mx_end() const noexcept {
        return max_set.end();
    }

    size_type size() const noexcept {
        return pts_set.size();
    }
};


template<typename A, typename V>
void FunctionMaxima<A, V>::set_value(const A &a, const V &v) {
    iterator iter = find(a);

    if (iter == end() || (!(v < iter->value()) && !(iter->value() < v))) {
        InsertGuard<pts_set_t> pts_set_guard{&pts_set};
        InsertGuard<max_set_t> left_max_set_guard{&max_set};
        InsertGuard<max_set_t> middle_max_set_guard{&max_set};
        InsertGuard<max_set_t> right_max_set_guard{&max_set};

        iterator it;
        if (iter == end()) {
            it = pts_set_guard.insert(point_type{a, v});
        }
        else {
            it = pts_set_guard.insert(point_type{&iter->arg(), v});
        }
        mx_iterator mx_iter = max_set.find(point_type{&a});

        bool has_left = it != begin() && (std::prev(it) != iter || iter != begin());
        bool has_right = std::next(it) != end()
                         && (std::next(it) != iter || std::next(iter) != end());

        if (has_left) {
            iterator middle =
                    std::prev(it) != iter ? std::prev(it) : std::prev(it, 2);
            iterator left = middle != begin() ? std::prev(middle) : middle;
            iterator right = std::next(it) != iter ? std::next(it) :
                             (std::next(iter) == end() ? iter : std::next(iter));

            if (is_local_max(left, middle, right) && !mx_set_contains(middle)) {
                left_max_set_guard.insert(*middle);
            }
        }

        {
            iterator left = has_left ? (std::prev(it) != iter
                                        ? std::prev(it) : std::prev(it, 2)) : it;
            iterator middle = it;
            iterator right = has_right ? (std::next(it) != iter ? std::next(it)
                                                                : std::next(it, 2)) :
                             (std::next(it) == iter ? std::next(it, 2) : std::next(
                                     it));

            if (is_local_max(left, middle, right) && !mx_set_contains(middle)) {
                middle_max_set_guard.insert(*middle);
            }
        }

        if (has_right) {
            iterator left = has_left ? (std::prev(it) != iter ? std::prev(it)
                                                              : std::prev(it, 2))
                                     : it;
            iterator middle =
                    std::next(it) != iter ? std::next(it) : std::next(it, 2);
            iterator right = std::next(middle) != end() ? std::next(middle) : middle;

            if (is_local_max(left, middle, right) && !mx_set_contains(middle)) {
                right_max_set_guard.insert(*middle);
            }
        }

        if (has_left) {
            iterator middle =
                    std::prev(it) != iter ? std::prev(it) : std::prev(it, 2);
            iterator left = middle != begin() ? std::prev(middle) : middle;
            iterator right = std::next(it) != iter ? std::next(it) :
                             (std::next(iter) == end() ? iter : std::next(iter));

            if (!is_local_max(left, middle, right) && mx_set_contains(middle)) {
                max_set.erase(middle);
            }
        }

        if (has_right) {
            iterator left = has_left ? (std::prev(it) != iter ? std::prev(it)
                                                              : std::prev(it, 2))
                                     : it;
            iterator middle =
                    std::next(it) != iter ? std::next(it) : std::next(it, 2);
            iterator right = std::next(middle) != end() ? std::next(middle) : middle;

            if (!is_local_max(left, middle, right) && mx_set_contains(middle)) {
                max_set.erase(middle);
            }
        }

        if (iter != end()) {
            if (mx_iter != mx_end()) {
                max_set.erase(mx_iter);
            }
            pts_set.erase(iter);
        }

        pts_set_guard.drop_rollback();
        left_max_set_guard.drop_rollback();
        middle_max_set_guard.drop_rollback();
        right_max_set_guard.drop_rollback();
    }
}

template<typename A, typename V>
void FunctionMaxima<A, V>::erase(const A &a) {
    if (size() == 1) {
        max_set.clear();
        pts_set.clear();
    }
    else if (size() > 0) {
        iterator it = find(a);
        bool has_left = it != begin();
        bool has_right = std::next(it) != end();
        mx_iterator mx_it = max_set.find(point_type{&a});
        bool was_local_max = mx_it != mx_end();
        InsertGuard<max_set_t> left_guard{&max_set}, right_guard{&max_set};

        if (has_left) {
            iterator middle = std::prev(it);
            iterator left = middle != begin() ? std::prev(middle) : middle;
            iterator right = std::next(it);

            if (is_local_max(left, middle, right) && !mx_set_contains(middle)) {
                left_guard.insert(*middle);
            }
        }

        if (has_right) {
            iterator left = has_left ? std::prev(it) : std::next(it);
            iterator middle = std::next(it);
            iterator right = middle != end() ? std::next(middle) : middle;

            if (is_local_max(left, middle, right) && !mx_set_contains(middle)) {
                right_guard.insert(*middle);
            }
        }

        pts_set.erase(it);
        if (was_local_max) {
            max_set.erase(mx_it);
        }

        left_guard.drop_rollback();
        right_guard.drop_rollback();
    }
}

#endif // FUNCTION_MAXIMA_H
