#include<cstdio>
#include<vector>

using namespace std;

const int infinity = 2000000001;
vector<int> data;

void minHeapify(vector<int> & A, int index);
int minExtract(vector<int> & A);
void insert(vector<int> & A, int key);
void decreaseKey(vector<int> & A, int index, int newKey);

int parent(int i) { return i / 2; }
int left(int i) { return 2 * i; }
int right(int i) { return 2 * i + 1; }
void debug(vector<int> A) { for(vector<int>::iterator i = A.begin(); i < A.end(); i++) { printf(" %d", *i); } printf("\n"); }

int main() {
  int Z; scanf("%d", &Z);
  while (Z--) {
    int N, x; scanf("%d", &N);
    for (int i = 0; i < N; i++) {  
      scanf("%d", &x);
      insert(data, x);
    }

    for (int i = 0; i < N; i++) {  
      printf("%d ", minExtract(data));
    }
    printf("\n");

    data.clear();
  }
  return 0;
}

void insert(vector<int> & A, int key) {
  if (A.size() == 0) A.push_back(-infinity); //so it is 1 base indexed, also -infinity is a guard here

  A.push_back(infinity);
  decreaseKey(A, A.size() - 1, key);
}

int minExtract(vector<int> & A) {
  int result = A[1];

  A[1] = A[A.size() - 1]; A.pop_back();
  minHeapify(A, 1);
  return result;
}

void decreaseKey(vector<int> & A, int index, int newKey) {
  if (A.at(index) <= newKey) return; //error actually

  A.at(index) = newKey;
  
  int pIndex = parent(index);
  while (A.at(pIndex) > A.at(index)) {
    int t = A.at(pIndex);
    A.at(pIndex) = A.at(index);
    A.at(index) = t; //swap

    index = pIndex;
    pIndex = parent(index);
  }
}

int min(int x, int y) { return x < y ? x : y; }
int at(vector<int> & A, int index) { if (index >= A.size()) return infinity; else return A[index]; }
void swap(int & x, int & y) { int t = x; x = y; y = t; }
void minHeapify(vector<int> & A, int index) {
  int lVal = at(A, left(index)), rVal = at(A, right(index));
  int smallest = min(lVal, rVal);
  if (A[index] <= smallest) return;

  if (lVal < rVal) {
    swap(A[left(index)], A[index]); 
    minHeapify(A, left(index));
  } else {
    swap(A[right(index)], A[index]); 
    minHeapify(A, right(index));
  }
}
