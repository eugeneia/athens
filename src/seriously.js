/*
  seriously.js: Provide basic functions missing in JavaScript.

    „Seriously, JavaScript?“ -- Hence the name.
*/

/* first: Return first item of vector. */
function first (vector) {
    return vector[0];
}

/* rest: Return tail of vector. */
function rest (vector) {
    if (vector.length < 2) return null;
    else return Array.prototype.slice.call(vector, 1);
}

/* last: Return last n items of vector (n defaults to 1). */
function last (vector, n) {
    if (n === undefined) n = 1;
    if (! (n > 0)) throw 'last: n must be > 0.';;

    return Array.prototype.slice.call(vector, vector.length - n);
}

/* last1: Return last item of vector. */
function last1 (vector) {
    return first(last(vector));
}

/* butlast: Return items of vector but the last n (n defaults
   to 1). */
function butlast (vector, n) {
    if (n === undefined) n = 1;
    if (! (n > 0)) throw 'last: n must be > 0.';

    return Array.prototype.slice.call(vector, 0, vector.length - n);
}

/* apply: Apply f on arguments. */
function apply (f) {
    return f.apply(null,
                   butlast(rest(arguments)).concat(last1(arguments)));
}

/* map: Map f over vectors. */
function map (f) {
    var
    vectors, length, result, i;

    vectors = rest(arguments);

    if (vectors === null) return [];

    length = first(vectors).length;
    result = [];

    for (i = 0; i < length; i += 1) {
        result.push(
            apply(f, vectors.map(function (v) {
                // Once here has been an awesome JavaScript bug. Can you
                // guess what it was?
                return v[i] !== undefined ? v[i] : null;
            }))
        );
    }

    return result;
}

/* reduce: Reduce vector using f. */
function reduce (f, vector) {
    var
    reduction;

    reduction = first(vector);

    map(function (item) { reduction = f(reduction, item); },
        rest(vector));

    return reduction;
}

/* eql: Are x and y equal in terms of === ? */
function eql (x, y) {
    return x === y;
}

/* findIf: Return first item from vector that satisfies test. */
function findIf (test, vector) {
    var i, length;

    for (i = 0, length = vector.length; i < length; i += 1)
        if (test(vector[i])) return vector[i];

    return null;
}

/* find: Return first item from vector that is equal to item according to
   test which defaults to eql. */
function find (item, vector, test) {
    if (!test) test = eql;

    return findIf(function (item2) { return test(item, item2); },
                  vector);
}

/* removeIf: Remove items from vector that satisfy predicate. */
function removeIf (predicate, vector) {
    var
    i, length, newVector;

    newVector = [];

    for (i = 0, length = vector.length; i < length; i += 1)
        if (!predicate(vector[i])) newVector.push(vector[i]);

    return newVector;
}

/* removeIfNot: Remove items from vector that do not satisfy
   predicate. */
function removeIfNot (predicate, vector) {
    return removeIf(function (x) { return !predicate(x); },
                    vector);
}

/* remove: Remove all occurences of item from vector using test which
   defaults to eql. */
function remove (item, vector, test) {
    if (!test) test = eql;

    return removeIf(function (x) { return test(x, item); },
                    vector);
}

/* removeDuplicates: Remove duplicates from vector using test which
   defaults to eql. */
function removeDuplicates (vector, test) {
    var
    i, length, newVector;

    if (!test) test = eql;

    newVector = [];

    for (i = 0, length = vector.length; i < length; i += 1)
        // !0 => true, remember that!
        if (find(vector[i], vector.slice(0, i), test) === null)
            newVector.push(vector[i]);

    return newVector;
}

/* gensym: Generate a unique string with prefix which defaults to 'G'.
   CAUTION: This will fail as soon as ____gensymCounter____ wraps
   around. */
var ____gensymCounter____ = 0;
function gensym (prefix) {
    var
    s;

    if (!prefix) prefix = 'G';

    s = prefix + ____gensymCounter____;

    ____gensymCounter____ += 1;

    return s;
}

/* iota: Return vector of n successive numbers starting at start and
   increasing by step. start defaults to 0 and step defaults to 1.*/
function iota (n, step, start) {
    var
    i, v, vector;

    if (!step) step = 1;
    if (!start) start = 0;

    for (i = 0, v = start, vector = []; i < n; i += 1, v += step)
        vector.push(v);

    return vector;
}
