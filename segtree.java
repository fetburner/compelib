import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.BinaryOperator;

/* セグ木を構成するADT */
interface RawSegTree<T>
{
	/* 要素全てを半群の演算子で畳み込んだもの */
	T data();
	/* 要素数 */
	int size();
	/* 添字[l, r)の要素を半群の演算子で畳み込んだ結果を返す */
	T query(BinaryOperator<T> op, int l, int r);
	/* 添字iの要素x_iをf(x_i)で更新する */
	RawSegTree<T> update(BinaryOperator<T> op, int i, Function<T, T> f);
}

/* セグ木の葉を作るやつ */
class Leaf<T> implements RawSegTree<T>
{
	/* この葉が保持する要素 */
	private final T x;

	public Leaf(T x) { this.x = x; }

	@Override public T data() { return x; }
	@Override public int size() { return 1; }

	@Override public T query(BinaryOperator<T> op, int l, int r) { return x; }
	@Override public Leaf update(BinaryOperator<T> op, int i, Function<T, T> f)
	{ return new Leaf(f.apply(x)); }
}

/* セグ木のノードを作るやつ */
class Node<T> implements RawSegTree<T>
{
	/* このノードが保持する要素
	   (=セグ木の要素全てを半群の演算子で畳み込んだもの) */
	private final T x;
	/* このノード以下の木にいくつ葉が含まれるか(=セグ木の要素数) */
	private final int n;
	/* このノードの子 */
	private final RawSegTree<T> t1, t2;

	public Node(BinaryOperator<T> op, RawSegTree<T> t1, RawSegTree<T> t2)
	{
		this.t1 = t1;
		this.t2 = t2;
		this.n = t1.size() + t1.size();
		this.x = op.apply(t1.data(), t2.data());
	}

	@Override public T data() { return x; }
	@Override public int size() { return n; }

	@Override public T query(BinaryOperator<T> op, int l, int r)
	{
		if (l == 0 && r == n) {
			return x;
		} else if (r <= t1.size()) {
			return t1.query(op, l, r);
		} else if (t1.size() <= l) {
			return t2.query(op, l - t1.size(), r - t1.size());
		} else {
			return op.apply(t1.query(op, l, t1.size()), t2.query(op, 0, r - t1.size()));
		}
	}

	@Override public Node update(BinaryOperator<T> op, int i, Function<T, T> f)
	{
		if (i < t1.size()) {
			return new Node(op, t1.update(op, i, f), t2);
		} else {
			return new Node(op, t1, t2.update(op, i - t1.size(), f));
		}
	}
}

/* セグ木 */
class SegTree<T>
{
	private final RawSegTree<T> t;
	private final BinaryOperator<T> op;

	private SegTree(BinaryOperator<T> op, RawSegTree<T> t) { this.op = op; this.t = t; }

	private RawSegTree<T> mkRawSegTree(int offset, int n, IntFunction<T> f)
	{
		if (n <= 1) {
			return new Leaf(f.apply(offset));
		} else {
			return new Node(op,
				mkRawSegTree(offset, n / 2, f),
				mkRawSegTree(offset + n / 2, (n + 1) / 2, f));
		}
	}

	/* 半群の演算子，要素数，添字についての関数からセグ木を作るコンストラクタ */
	public SegTree(BinaryOperator<T> op, int n, IntFunction<T> f) throws IllegalArgumentException
	{
		if (n <= 0) {
			throw new IllegalArgumentException();
		} else {
			this.op = op;
			this.t = mkRawSegTree(0, n, f);
		}
	}

	public T query(int l, int r) throws IndexOutOfBoundsException
	{
		if (l < 0 || t.size() <= r) {
			throw new IndexOutOfBoundsException();
		} else {
			return t.query(op, l, r);
		}
	}

	public SegTree update(int i, Function<T, T> f) throws IndexOutOfBoundsException
	{
		if (i < 0 || t.size() <= i) {
			throw new IndexOutOfBoundsException();
		} else {
			return new SegTree(op, t.update(op, i, f));
		}
	}
}
