#include "Lambda.h"
#include <algorithm>

Ap::Ap(std::vector<Lambda>&& terms) : terms{} {
	if (Ap* t0 = std::get_if<Ap>(&terms[0]); t0) {
		this->terms = std::move(t0->terms);
		for (size_t i = 1; i < terms.size(); ++i) {
			this->terms.emplace_back(std::move(terms[i]));
		}
	} else {
		this->terms = std::move(terms);
	}
}

Ap::Ap(std::initializer_list<Lambda> terms) : Ap(std::vector(terms)) {} // not-so-uniform initialization

L::L(unsigned k, Lambda&& term) : k{ k } {
	vassert(k > 0 && "There should be at least one bound variable!");
	if (L* m1 = std::get_if<L>(&term); m1) {
		this->k += m1->k;
		this->term = std::make_unique<Lambda>(std::move(*m1));
	} else {
		this->term = std::make_unique<Lambda>(std::move(term));
	}
}

// Prints a generic name from the list: u,v,w,x,y,z,u0,v0,w0,...
void printGenericName(std::ostream& os, const unsigned i) {
	os << char('u' + i % 6);
	if (i >= 6) {
		os << (i / 6) - 1;
	}
}

// A context gives a mapping from [0;n) to arbitrary names. Each new bound variable is
// pushed to the front of the context, bumping the indices of the previously bound variables.
// An implicit context maps to the numbers in [0;n) in reverse order (because of this index bumping).
class ImplicitContext {
	unsigned n;
public:
	ImplicitContext() : n{ 0 } {}
	~ImplicitContext() {
		vassert(n == 0 && "Error: context should be empty after printing");
	}
	// Print the mapped name of a given variable.
	void printName(std::ostream& os, const unsigned idx) const {
		// There term may have free variables - for them the context has no mapping
		if (idx < n) {
			// We "flip" the index due to the reverse order of the numbers
			printGenericName(os, n - 1 - idx);
		} else {
			printGenericName(os, idx);
		}
	}
	// Push k new variables
	void push(const unsigned k) {
		n += k;
	}
	// Pop the k newest variables
	void pop(const unsigned k) {
		n -= k;
	}
	unsigned size() const {
		return n;
	}
};

// The named context maps the first bound variables to given strings, but otherwise behaves as an
// implicit context. For named contexts pushing and popping expand and contract the visible "space"
// of names. Of course, once the given names run out, the implicit context behaviour will kick in.
class NamedContext : public ImplicitContext {
	const std::vector<std::string>& names;
public:
	NamedContext(const std::vector<std::string>& names) : ImplicitContext{}, names { names } {}
	// Print the mapped name of a given variable.
	void printName(std::ostream& os, const unsigned idx) const {
		// We remap only the outermost variables (the ones with the highest DeBruijn indices).
		// This means the highest indices should map to indices [0..] in the names array.
		const unsigned n = ImplicitContext::size();
		const unsigned idx1 = n - 1 - idx;
		if (idx1 < names.size()) {
			os << names[idx1];
		} else {
			ImplicitContext::printName(os, idx + unsigned(names.size()));
		}
	}
};

template <class Context>
struct PrintVisitor {
	std::ostream& os;
	Context& ctx;

	PrintVisitor(std::ostream& os, Context& ctx) : os{ os }, ctx{ ctx } {}
	// The visiting functions:
	void operator()(const Var& t) {
		ctx.printName(os, t.idx);
	}
	void operator()(const Ap& t) {
		for (const auto& term : t.terms) {
			// Put brackets only around the complex terms
			const bool needsBr = !std::get_if<Var>(&term);
			if (needsBr) { os << '('; }
			std::visit(*this, term); // Use the same visitor, to preserve the context
			if (needsBr) { os << ')'; }
		}
	}
	void operator()(const L& t) {
		// Push the names of the new k bound variables into the context
		ctx.push(t.k);
		os << '\\';
		for (unsigned k = t.k; k > 0; --k) {
			ctx.printName(os, k - 1);
		}
		os << '.';
		std::visit(*this, *t.term);
		// It is obligatory to pop the new names from the context afterwards.
		ctx.pop(t.k);
	}
};

std::ostream& operator<<(std::ostream& os, const Lambda& l) {
	ImplicitContext ctx{};
	std::visit(PrintVisitor{ os,ctx }, l);
	return os;
}

void printWithCtx(std::ostream& os, const Lambda& l, const std::vector<std::string>& names) {
	NamedContext ctx{ names };
	std::visit(PrintVisitor{ os,ctx }, l);
	os << '\n';
}
