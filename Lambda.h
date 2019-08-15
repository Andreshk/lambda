#pragma once
#include <iostream>
#include <variant>
#include <vector>
#include <string_view>

#include <cassert>
#define vassert assert

// Forward declaration
class Lambda;

// A lambda term is either:
// 1. A variable, denoted by its Debruijn index
struct Var {
	unsigned idx;
};
// 2. An application of a term and one or more arguments (all kept together in a vector)
struct Ap {
	std::vector<Lambda> terms;
	Ap(std::initializer_list<Lambda> terms);
};
// 3. An abstraction, binding one or more variables
struct L {
	unsigned k;
	std::unique_ptr<Lambda> body;
	L(unsigned k, Lambda&& body);
	// A copy ctor and operator= are required
	L(const L& other) : k{ other.k }, body{ std::make_unique<Lambda>(*other.body) } {}
	L& operator=(const L& other) {
		if (this != &other) {
			this->~L();
			new (this) L(other);
		}
		return *this;
	}
};

// We can now define a lambda as a union of the three elementary types.
class Lambda : std::variant<Var, Ap, L> {
	using Base = std::variant<Var, Ap, L>;
	using Base::Base; // Use all of std::variant's ctors and assignment operators
	using Base::operator=;
public:
	// std::visit is SFINAE-restricted to specializations of std::variant -> so we make our own visit
	template <class Visitor, class... Lambdas>
	static decltype(auto) visit(Visitor&& v, const Lambdas& ...ls) {
		return std::visit(std::forward<Visitor>(v), reinterpret_cast<const Base&>(ls)...);
	}
	// The private inheritance bars the conversion from Lambda* to Base* -> so we make our own get_if, too
	template <typename T>
	static T* get_if(Lambda* l) { return std::get_if<T>(reinterpret_cast<Base*>(l)); }
	template <typename T>
	static const T* get_if(const Lambda* l) { return std::get_if<T>(reinterpret_cast<const Base*>(l)); }

private:
	// Perform one beta-reduction step using the normal reduction strategy
	// (reduce the outermost, left-most redex). Returns true if no regex is found.
	bool betaStep();
public:
	// Beta reduction. Currently does not recognize terms that reduce to themselves.
	// Returns the number of steps needed (i.e. the number of reduced regexes).
	unsigned betaReduce();

	// Check whether the term is properly compressed (see Ap::Ap() and L::L())
	bool isValid() const;
};

// Pretty-print a given lambda term, using automatically generated names.
std::ostream& operator<<(std::ostream& os, const Lambda& l);

// Pretty-print a given lambda term, using the given names for the outermost bound variables.
void printWithCtx(std::ostream& os, const Lambda& l, const std::vector<std::string_view>& names);

// Lambda term alpha-equivalence
bool operator==(const Lambda& l1, const Lambda& l2);
bool operator!=(const Lambda& l1, const Lambda& l2);
