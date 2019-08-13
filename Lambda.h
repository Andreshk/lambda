#pragma once
#include <iostream>
#include <variant>
#include <vector>
#include <string>

#include <cassert>
#define vassert assert

using Lambda = std::variant<struct Var, struct Ap, struct L>;

struct Var {
	unsigned idx;
};

struct Ap {
	std::vector<Lambda> terms;
	Ap(std::vector<Lambda>&& terms);
	Ap(std::initializer_list<Lambda> terms);
};

struct L {
	unsigned k;
	std::unique_ptr<Lambda> term;
	L(unsigned k, Lambda&& term);
	// A copy ctor is required
	L(const L& other) : k{ other.k }, term{ other.term ? std::make_unique<Lambda>(*other.term) : nullptr } {}
};

std::ostream& operator<<(std::ostream& os, const Lambda& l);

void printWithCtx(std::ostream& os, const Lambda& l, const std::vector<std::string>& names);
