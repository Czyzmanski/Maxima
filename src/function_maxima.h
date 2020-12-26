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
    public:
        ~point_type() = default;

        point_type(const point_type &other) = default;

        point_type(point_type &&other) noexcept = default;

        point_type &operator=(const point_type &other) = default;

        point_type &operator=(point_type &&other) noexcept = default;

        const A &arg() const {
            return a_ptr.get();
        }

        const V &value() const {
            return v_ptr.get();
        }

    private:
        using a_ptr_t = std::shared_ptr<A>;
        using v_ptr_t = std::shared_ptr<V>;

        a_ptr_t a_ptr;
        v_ptr_t v_ptr;

        explicit point_type(const A &a) : a_ptr(std::make_shared<A>(a)) {}

        point_type(const A &a, const V &v) : a_ptr(std::make_shared<A>(a)),
                                             v_ptr(std::make_shared<V>(v)) {}
    };


    FunctionMaxima() noexcept: pts_set(point_type::cmp_by_arg),
                               max_set(point_type::cmp_by_val_then_by_arg) {};

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
    static inline auto cmp_by_arg = [](const point_type &pt_1,
                                       const point_type &pt_2) {
        return pt_1.arg() < pt_2.arg();
    };

    static inline auto cmp_by_val_then_by_arg = [](const point_type &pt_1,
                                                   const point_type &pt_2) {
        if (!(pt_1.value() < pt_2.value()) && !(pt_2.value() < pt_1.value())) {
            return cmp_by_arg(pt_1, pt_2);
        }
        else {
            return pt_1.value() < pt_2.value();
        }
    };

    using pts_set_t = std::set<point_type, decltype(cmp_by_arg)>;
    using max_set_t = std::set<point_type, decltype(cmp_by_val_then_by_arg)>;

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

        void insert(const point_type &pt) {
            it = set->insert(pt).first;
            rollback = true;
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
        return pts_set.find(point_type(a));
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


// Zmienia funkcję tak, żeby zachodziło f(a) = v. Jeśli a nie należy do
// obecnej dziedziny funkcji, jest do niej dodawany. Najwyżej O(log n).
template<typename A, typename V>
void FunctionMaxima<A, V>::set_value(const A &a, const V &v) {
    //TODO: implement
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
        bool has_right = it + 1 != end();
        mx_iterator mx_it = max_set.find(a);
        bool was_local_max = mx_it != mx_end();
        InsertGuard<max_set_t> left_guard{max_set}, right_guard{max_set};

        if (has_left) {
            iterator middle = it - 1;
            iterator left = middle != begin() ? middle - 1 : middle;
            iterator right = it + 1;

            if (is_local_max(left, middle, right)) {
                left_guard.insert(*middle);
            }
        }

        if (has_right) {
            iterator left = has_left ? it - 1 : it + 1;
            iterator middle = it + 1;
            iterator right = middle != end() ? middle + 1 : middle;

            if (is_local_max(left, middle, right)) {
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
