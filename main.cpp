#pragma comment(lib, "bdd.lib")
#include "bdd.h"

#include <iostream>
#include <cmath>
#include <vector>

const std::size_t N = 9;
const std::size_t M = 4;
const std::size_t K = 21;
const std::size_t ROW_LENGTH = 3;
const int LOG_N = static_cast<int>(std::ceil(std::log2(N)));

const std::size_t N_VAR = M * N * LOG_N;
std::vector<char> var;

enum Neighbour {
	RIGHT_TOP,
	RIGHT_BOTTOM
};

struct Object {
	Object(int propertyNumber, int objectNumber, int propertyValue) :
		propertyNumber_(propertyNumber), listNumber_(objectNumber), propertyValue_(propertyValue) {}

	int propertyNumber_;
	int listNumber_;
	int propertyValue_;
};

void init(bdd p[M][N][N]) {
	var.resize(N_VAR);

	for (int i = 0; i < M; ++i) {
		for (int j = 0; j < N; ++j) {
			for (int k = 0; k < N; ++k) {
				p[i][j][k] = bddtrue;
				for (int l = 0; l < LOG_N; ++l) {
					p[i][j][k] &= ((k >> l) & 1) ? bdd_ithvar((j * LOG_N * M) + l + LOG_N * i) :
						bdd_nithvar((j * LOG_N * M) + l + LOG_N * i);
				}
			}
		}
	}
}

unsigned calculateSum(unsigned* propertiesArr, unsigned arrSize) {
	unsigned sum = 0;
	for (unsigned i = 0; i < arrSize; i++) {
		sum += propertiesArr[i];
	}
	return sum;
}

void incrementVector(unsigned* arr, int neighbours) {
	arr[0] += 1;
	for (unsigned i = 0; i < M * neighbours; i++) {
		if (arr[i] == N) {
			if (i == M * neighbours - 1) {
				for (unsigned j = 0; j < M * neighbours; j++) {
					arr[j] = 0;
				}
			}
			else {
				arr[i] = 0;
				arr[i + 1] += 1;
			}
		}
	}
}

bdd cond1(const bdd p[M][N][N], const Object Object) {
	return p[Object.propertyNumber_ - 1][Object.listNumber_ - 1][Object.propertyValue_ - 1];
}

bdd cond2(const bdd p[M][N][N], const Object Object1, const Object Object2) {
	bdd tree = bddtrue;
	for (int i = 0; i < N; i++) {
		tree &= !(p[Object1.propertyNumber_ - 1][i][Object1.propertyValue_ - 1] ^
			p[Object2.propertyNumber_ - 1][i][Object2.propertyValue_ - 1]);
	}
	return tree;
}

bdd cond3(const bdd p[M][N][N], const Neighbour neighbour, const Object curentProperty, Object neighbourProperty) {
	bdd tree = bddtrue;
	switch (neighbour) {
	case RIGHT_TOP: {
		for (int i = ROW_LENGTH; i < N; ++i) {
			if ((i + 1) % ROW_LENGTH != 0) {
				tree &= !(p[curentProperty.propertyNumber_ - 1][i][curentProperty.propertyValue_ - 1] ^
					p[neighbourProperty.propertyNumber_ - 1][i - ROW_LENGTH + 1][neighbourProperty.propertyValue_ - 1]);
			}
		}
		break;
	}
	case RIGHT_BOTTOM: {
		for (int i = 0; i < N - (ROW_LENGTH + 1); ++i) {
			if ((i + 1) % ROW_LENGTH != 0) {
				tree &= !(p[curentProperty.propertyNumber_ - 1][i][curentProperty.propertyValue_ - 1] ^
					p[neighbourProperty.propertyNumber_ - 1][i + ROW_LENGTH + 1][neighbourProperty.propertyValue_ - 1]);
			}
		}
		break;
	}
	}
	return tree;
}

bdd cond4(const bdd p[M][N][N], const Object currentProperty, const Object neihbourProperty) {
	bdd tree = bddfalse;
	std::vector<Neighbour> neighbours;
	neighbours.push_back(Neighbour::RIGHT_BOTTOM);
	neighbours.push_back(Neighbour::RIGHT_TOP);

	for (Neighbour neighbour : neighbours) {
		tree |= cond3(p, neighbour, currentProperty, neihbourProperty);
	}

	return tree;
}

bdd cond5(bdd& tree, const bdd p[M][N][N]) {
	for (unsigned j = 0; j < N; j++) {
		for (unsigned i = 0; i < N - 1; i++) {
			for (unsigned k = i + 1; k < N; k++) {
				for (unsigned m = 0; m < M; m++) {
					tree &= p[m][i][j] >> !p[m][k][j];
				}
			}
		}
	}
	return tree;
}

bdd cond6(const bdd p[M][N][N]) {
	bdd tree = bddtrue;

	for (int i = 0; i < N; i++) {
		bdd temp[M];
		for (int m = 0; m < M; m++) {
			temp[m] = bddfalse;
		}
		for (unsigned j = 0; j < N; j++) {
			for (int m = 0; m < M; m++) {
				temp[m] |= p[m][i][j];
			}
		}
		for (int m = 0; m < M; m++) {
			tree &= temp[m];
		}
	}
	return tree;
}

void cond7(const bdd p[M][N][N], bdd& tree)
{
	for (unsigned i = 0; i < N; i++)
	{
		if (i % ROW_LENGTH == 2)
			continue;
		if (i == 6 || i == 7) {
			unsigned arr[M] = {};
			bdd constraint = bddfalse;
			for (unsigned step = 0; step < pow(N, M); step++) {
				bdd tempp = bddtrue;

				if (calculateSum(arr, M) < K) {
					for (unsigned k = 0; k < M; k++) {
						tempp &= p[k][i - 2][arr[k]];
					}
					constraint |= tempp;
				}
				incrementVector(arr, 1);
			}
			tree &= constraint;
		}

		else if (i == 0 || i == 1) {
			unsigned arr[M] = {};
			bdd constraint = bddfalse;
			for (unsigned step = 0; step < pow(N, M); step++) {
				bdd tempp = bddtrue;

				if (calculateSum(arr, M) < K) {
					for (unsigned k = 0; k < M; k++) {
						tempp &= p[k][i + 4][arr[k]];
					}
					constraint |= tempp;
				}
				incrementVector(arr, 1);
			}
			tree &= constraint;
		}
		else {
			unsigned arr[2 * M] = {};
			bdd constraint = bddfalse;
			for (unsigned step = 0; step < pow(N, 2 * M); step++) {
				bdd tempp = bddtrue;
				if (calculateSum(arr, 2 * M) < K) {
					for (unsigned k = 0; k < M; k++) {
						tempp &= p[k][i - 2][arr[k]];
						tempp &= p[k][i + 4][arr[k + M]];
					}
					constraint |= tempp;
				}
				incrementVector(arr, 2);
			}
			tree &= constraint;
		}
	}
}

void print() {
	for (unsigned i = 0; i < N; i++) {
		std::cout << i + 1 << ": ";
		for (unsigned j = 0; j < M; j++) {
			unsigned J = i * M * LOG_N + j * LOG_N;
			unsigned num = 0;
			for (unsigned k = 0; k < LOG_N; k++)
				num += (unsigned)(var[J + k] << k);
			std::cout << num + 1 << ' ';
		}
		std::cout << "\t";
		if (((i + 1) % 3) == 0) {
			std::cout << "\n";
		}
	}
	std::cout << "\n";
}

void build(char* varset, unsigned n, unsigned I) {
	if (I == n - 1) {
		if (varset[I] >= 0) {
			var[I] = varset[I];
			print();
			return;
		}
		var[I] = 0;
		print();
		varset[I] = 1;
		print();
		return;
	}
	if (varset[I] >= 0) {
		var[I] = varset[I];
		build(varset, n, I + 1);
		return;
	}
	var[I] = 0;
	build(varset, n, I + 1);
	var[I] = 1;
	build(varset, n, I + 1);
}

void fun(char* varset, int size) {
	build(varset, size, 0);
}

void program() {

	bdd_init(10000000, 1000000);
	bdd_setvarnum(N_VAR);
	bdd p[M][N][N];

	init(p);

	bdd tree = bddtrue;

	std::cout << "After 0: " << bdd_satcount(tree) << std::endl;

	tree &= cond6(p);

	std::cout << "After 6: " << bdd_satcount(tree) << std::endl;

	tree &= cond1(p, Object(1, 1, 9));
	tree &= cond1(p, Object(1, 2, 4));

	tree &= cond1(p, Object(1, 3, 5));
	tree &= cond1(p, Object(2, 8, 6));
	tree &= cond1(p, Object(3, 7, 7));
	tree &= cond1(p, Object(4, 9, 6));
	tree &= cond1(p, Object(1, 9, 1));
	tree &= cond1(p, Object(1, 5, 3));
	tree &= cond1(p, Object(2, 2, 4));
	tree &= cond1(p, Object(3, 4, 8));
	tree &= cond1(p, Object(4, 6, 5));
	tree &= cond1(p, Object(3, 9, 6));
	tree &= cond1(p, Object(4, 1, 9));

	std::cout << "After 1: " << bdd_satcount(tree) << std::endl;

	tree &= cond2(p, Object(3, -1, 8), Object(2, -1, 8));
	tree &= cond2(p, Object(4, -1, 4), Object(3, -1, 4));
	tree &= cond2(p, Object(2, -1, 7), Object(1, -1, 7));
	tree &= cond2(p, Object(3, -1, 2), Object(2, -1, 5));
	tree &= cond2(p, Object(4, -1, 1), Object(3, -1, 1));
	tree &= cond2(p, Object(2, -1, 2), Object(1, -1, 2));

	tree &= cond2(p, Object(3, -1, 9), Object(4, -1, 9));
	tree &= cond2(p, Object(3, -1, 8), Object(4, -1, 8));

	std::cout << "After 2: " << bdd_satcount(tree) << std::endl;

	tree &= cond3(p, Neighbour::RIGHT_TOP, Object(2, -1, 8), Object(4, -1, 3));
	tree &= cond3(p, Neighbour::RIGHT_BOTTOM, Object(1, -1, 3), Object(3, -1, 6));
	tree &= cond3(p, Neighbour::RIGHT_TOP, Object(1, -1, 7), Object(2, -1, 3));
	tree &= cond3(p, Neighbour::RIGHT_BOTTOM, Object(3, -1, 8), Object(3, -1, 1));
	tree &= cond3(p, Neighbour::RIGHT_BOTTOM, Object(2, -1, 4), Object(1, -1, 2));

	std::cout << "After 3: " << bdd_satcount(tree) << std::endl;

	tree &= cond4(p, Object(1, -1, 3), Object(2, -1, 5));
	tree &= cond4(p, Object(4, -1, 8), Object(1, -1, 4));
	tree &= cond4(p, Object(1, -1, 7), Object(1, -1, 3));

	std::cout << "After 4: " << bdd_satcount(tree) << std::endl;

	cond7(p, tree);

	std::cout << "After 7: " << bdd_satcount(tree) << std::endl;

	cond5(tree, p);

	std::cout << "After 5: " << bdd_satcount(tree) << std::endl;

	bdd_allsat(tree, fun);
	system("pause");
	std::cout << tree << "\n";
	system("pause");
	bdd_done();
}

int main() {
	program();
}