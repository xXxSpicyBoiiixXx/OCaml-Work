/*
 *	json::Saver class interface
 *
 *	This file is part of OTAWA
 *	Copyright (c) 2016, IRIT UPS.
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
#ifndef ELM_JSON_SAVER_H_
#define ELM_JSON_SAVER_H_

#include <elm/data/Vector.h>
#include <elm/io.h>
#include <elm/io/BufferedOutStream.h>
#include <elm/string/utf8.h>
#include <elm/sys/Path.h>
#include "common.h"

namespace elm { namespace json {

class Saver {
public:
	Saver(io::OutStream& out = io::out);
	Saver(StringBuffer& buf);
	Saver(sys::Path& path);
	~Saver(void);
	void close(void);

	inline bool isReadable(void) const { return readable; }
	inline void setReadable(bool read) { readable = read; }
	inline string getIndent(void) const { return indent; }
	inline void setIndent(string i) { indent = i; }

	void beginObject(void);
	void endObject(void);
	void beginArray(void);
	void endArray(void);
	void addField(string id);

	void put(void);
	inline void put(const char *val) { put(cstring(val)); }
	void put(cstring val);
	void put(string val);
	void put(t::uint64 val);
	void put(t::int64 val);
	inline void put(int val) { put(t::int64(val)); }
	void put(double val);
	void put(bool val);

private:
	typedef enum {
		BEGIN,
		OBJECT,
		IN_OBJECT,
		FIELD,
		ARRAY,
		IN_ARRAY,
		END
	} state_t;

	void doIndent(bool close = false);
	static state_t next(state_t s);
	static bool isObject(state_t s);
	static bool isArray(state_t s);
	void escape(utf8::char_t c);

	state_t state;
	Vector<state_t> stack;
	io::Output _out;
	bool readable;
	string indent;
	io::BufferedOutStream *buf;
	io::OutStream *str;
};

} }	// elm::json

#endif /* ELM_JSON_SAVER_H_ */
