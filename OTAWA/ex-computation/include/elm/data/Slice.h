/*
 *	Slice class
 *
 *	This file is part of OTAWA
 *	Copyright (c) 2020, IRIT UPS.
 *
 *	OTAWA is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	OTAWA is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OTAWA; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifndef ELM_DATA_SLICE_H_
#define ELM_DATA_SLICE_H_

#include <elm/PreIterator.h>

namespace elm {

template <class T, class D>
class Slice {
public:
	inline Slice(): a(nullptr), f(0), c(0) { }
	inline Slice(D& array, int first, int count): a(&array), f(first), c(count) { }

	D& array() const { return *a; }
	inline int firstIndex() const { return f; }
	inline int lastIndex() const { return f + c - 1; }
	inline int count() const { return c; }

	inline const T& get(int i) const { return (*a)[f + i]; }
	inline const T& operator[](int i) const { return get(i); }
	inline T& get(int i) { return (*a)[f + i]; }
	inline T& operator[](int i) { return get(i); }

	class Iter: public PreIterator<Iter, T> {
	public:
		inline Iter(const Slice& slice, int idx = 0): s(slice), i(idx) { }
		inline bool ended() const { return i >= s.c; }
		inline const T& item() const { return s[i]; }
		inline void next() { i++; }
		inline bool equals(const Iter& ii) const { return &s == &ii.s && i == ii.i; }
	private:
		const Slice& s;
		int i;
	};

	inline Iter begin() const { return Iter(*this); }
	inline Iter end() const { return Iter(*this, c); }

	// Array concept additions
	inline int length() const { return count(); }
	inline int indexOf(const T& x, int i = 0) const
		{ for(; i < c; i++) if(x == get(i)) return i; return -1; }
	inline int lastIndexOf(const T& x, int i = -1) const
		{ if(i == -1) i = c - 1; for(; i >= 0; i--) if(x == get(i)) break; return i; }

	// Collection concept additions
	inline bool contains(const T& x) const
		{ return indexOf(x) != -1; }
	inline bool containsAll(const Slice<T, D>& s) const
		{ for(auto x: s) if(!contains(x)) return false; return true; }
	inline bool isEmpty() const { return c == 0; }
	inline operator bool() const { return !isEmpty(); }
	inline bool equals(const Slice<T, D>& s) const {
		if(c != s.c) return false;
		for(int i = 0; i < c; i++) if(get(i) != s.get(i)) return false;
		return true;
	}
	inline bool operator==(const Slice<T, D>& s) const { return equals(s); }
	inline bool operator!=(const Slice<T, D>& s) const { return !equals(s); }

private:
	D *a;
	int f, c;
};

}	// elm

#endif /* ELM_DATA_SLICE_H_ */
