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
        auto pt = find(a);
        if (pt == end()) {
            throw InvalidArg();
        }
        else {
            return pt->value();
        }
    }

    void set_value(const A &a, const V &v);

    void erase(const A &a) {
        auto pt = find(a);
        if (pt != end()) {
            pts_set.erase(pt);
        }
    }

private:
    class SetValueGuard {
        //TODO: implement
    };

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

#endif // FUNCTION_MAXIMA_H
