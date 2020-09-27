#ifndef RECORD_C
#define RECORD_C

#define record(name, size) \
  void(^*record_predicates)(void) = (void(^[size])(void)){0}; \
  int current_record_pos = 0; \
  static int this_type = 0; \
  if (this_type==0) this_type = next_type++; \
  immutable void(^ cognate_function_ ## name)(void) = \
  ^{ \
    cognate_object *rec = (cognate_object*) GC_MALLOC (sizeof(cognate_object) * (size + 1)); \
    rec[size] = (cognate_object){.type=terminator}; \
    for (int i = 0; i < size; ++i) \
    { \
      if (record_predicates[i] == NULL) rec[i] = pop_any(); \
      else { \
        cognate_object item = peek_any(); \
        record_predicates[i](); \
        if (pop(boolean)) \
        { \
          rec[i] = check_block(item); \
        } \
        else throw_error("Item assertion failed for record '"#name"'"); \
      } \
    } \
    push_any((cognate_object){.type=this_type, .record=rec}); \
  }; \
  immutable void(^ cognate_function_ ## name ## _)(void) = \
  ^{ \
    push(boolean, pop_any().type == this_type); \
  }

// Field definitions MUST come directly after record definitions
// because each record definition increments the type counter.

#define field(name, predicate) \
  record_predicates[current_record_pos] = predicate; \
  const int record_ ## name ## _pos = current_record_pos++; \
  immutable void(^ cognate_function_ ## name)(void) = \
  ^{ \
    cognate_object rec = pop_any(); \
    if (rec.type == this_type) { push_any(rec.record[record_ ## name ## _pos]); } \
    else { throw_error("No field '"#name"' in record!"); } \
  };

#endif
