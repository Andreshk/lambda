#include "Lambda.h"

Ap::Ap(std::initializer_list<Lambda> terms) : terms{} {
	// The terms are "compressed" - a term will contain all of its arguments in the same vector.
	// This means that ((xy)z) will be repesented as a single node (xyz), and it should be
	// impossible for the first term (the "function" applied) to be an application itself.
	if (const Ap* t0 = Lambda::get_if<Ap>(terms.begin()); t0) {
		this->terms = t0->terms;
		for (auto it = terms.begin() + 1; it != terms.end(); ++it) {
			this->terms.emplace_back(std::move(*it));
		}
	} else {
		this->terms = terms;
	}
	vassert(this->terms.size() >= 2);
}

L::L(const unsigned k, Lambda&& body) : k{ k } {
	// Consecutive abstractions \x\y.(...) should also be compressed into a single node: \xy.(...)
	vassert(k > 0 && "There should be at least one bound variable!");
	if (L* term1 = Lambda::get_if<L>(&body); term1) {
		this->k += term1->k;
		this->body = std::move(term1->body);
	} else {
		this->body = std::make_unique<Lambda>(std::move(body));
	}
}

// Prints a generic name from the list: u,v,w,x,y,z,u0,v0,w0,...
void printGenericName(std::ostream& os, const unsigned idx) {
	os << char('u' + idx % 6);
	if (idx >= 6) {
		os << (idx / 6) - 1;
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
		// The term may have free variables - for them the context has no mapping
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
	// Current size of the context
	unsigned size() const {
		return n;
	}
};

// The named context maps the first bound variables to given strings, but otherwise behaves as an
// implicit context. For named contexts pushing and popping expand and contract the visible "space"
// of names. Of course, once the given names run out, the implicit context behaviour will kick in.
class NamedContext : public ImplicitContext {
	const std::vector<std::string_view>& names;
public:
	NamedContext(const std::vector<std::string_view>& names) : ImplicitContext{}, names { names } {}
	// Print the mapped name of a given variable.
	void printName(std::ostream& os, const unsigned idx) const {
		// We remap only the outermost variables (the ones with the highest DeBruijn indices).
		// This means the highest indices should map in reverse to indices [0..] in the names array.
		const unsigned idx1 = size() - 1 - idx;
		if (idx1 < names.size()) {
			os << names[idx1];
		} else {
			ImplicitContext::printName(os, idx + unsigned(names.size()));
		}
	}
};

// Visitor for pretty-printing a lambda term to a given output stream,
// parameterized by the context that's used to generate names for variable indices.
template <class Context>
struct PrintVisitor {
	std::ostream& os;
	Context& ctx;

	PrintVisitor(std::ostream& os, Context& ctx) : os{ os }, ctx{ ctx } {}
	// The visiting functions:
	void operator()(const Var& l) {
		ctx.printName(os, l.idx);
	}
	void operator()(const Ap& l) {
		for (const auto& term : l.terms) {
			// Put brackets only around the complex terms
			const bool needsBr = !Lambda::get_if<Var>(&term);
			if (needsBr) { os << '('; }
			Lambda::visit(*this, term); // Use the same visitor, to preserve the context
			if (needsBr) { os << ')'; }
		}
	}
	void operator()(const L& l) {
		// Push the names of the new k bound variables into the context
		ctx.push(l.k);
		os << '\\';
		for (unsigned k = l.k; k > 0; --k) {
			ctx.printName(os, k - 1);
		}
		os << '.';
		Lambda::visit(*this, *l.body);
		// It is obligatory to pop the new names from the context afterwards.
		ctx.pop(l.k);
	}
};

std::ostream& operator<<(std::ostream& os, const Lambda& l) {
	ImplicitContext ctx{};
	Lambda::visit(PrintVisitor{ os,ctx }, l);
	return os;
}

void printWithCtx(std::ostream& os, const Lambda& l, const std::vector<std::string_view>& names) {
	NamedContext ctx{ names };
	Lambda::visit(PrintVisitor{ os,ctx }, l);
	os << '\n';
}

// Visitor class for alpha-equivalence. Used for traversing two lambda terms in
// parallel, checking whether they have the same structure and the same content.
struct EquivVisitor {
	bool operator()(const Var& l1, const Var& l2) const {
		return (l1.idx == l2.idx);
	}
	bool operator()(const Ap& l1, const Ap& l2) const {
		if (l1.terms.size() != l2.terms.size()) {
			return false;
		}
		for (size_t i = 0; i < l1.terms.size(); ++i) {
			if (!Lambda::visit(*this, l1.terms[i], l2.terms[i])) {
				return false;
			}
		}
		return true;
	}
	bool operator()(const L& l1, const L& l2) const {
		return (l1.k == l2.k && Lambda::visit(*this, *l1.body, *l2.body));
	}
	// Catch-all clause (much more verbose than [](auto x, auto y){ ... })
	// for when std::visit does not find a better-matching clause.
	template <typename V1, typename V2>
	bool operator()(const V1&, const V2&) const {
		return false;
	}
};

bool operator==(const Lambda& l1, const Lambda& l2) {
	return Lambda::visit(EquivVisitor{}, l1, l2);
}
bool operator!=(const Lambda& l1, const Lambda& l2) {
	return !(l1 == l2);
}

// Normal reduction strategy, corresponding to lazy evaluation.
bool Lambda::betaStep() {
	vassert(false && "Not implemented!");
	return false;
}

unsigned Lambda::betaReduce() {
	unsigned numRedexes = 0;
	// Each beta-step reduces one redex.
	while (betaStep()) {
		++numRedexes;
	}
	return numRedexes;
}

bool Lambda::isValid() const {
	struct ValidVisitor {
		bool operator()(const Var& l) {
			return true; // A variable is always structurally valid
		}
		bool operator()(const Ap& l) {
			if (l.terms.size() < 2 || Lambda::get_if<Ap>(&l.terms[0])) {
				return false;
			}
			for (const Lambda& term : l.terms) {
				if (!Lambda::visit(*this, term)) {
					return false;
				}
			}
			return true;
		}
		bool operator()(const L& l) {
			const Lambda& body = *l.body;
			return (l.k > 0 && !Lambda::get_if<L>(&body) && Lambda::visit(*this, body));
		}
	};
	return Lambda::visit(ValidVisitor{}, *this);
}
