#include<cstdio>
using namespace std;

/* 
Heap can be seen as a tree and as an array
  1     0th level
 /  \
 2   3  1st level
/ \ / \
4 5 6 7 2nd level

[1,2,3,4,5,6,7]
nth level takes 2^n cells

parent(i) = i / 2; // parent of ith element
lch(i) = 2i; //left child
rch(i) = 2i; //right child

max heap invariant: A[Parent(i)] >= A[i]

max-heapify(A, i) //restore max heap in A[i] subtree                                    //O(log n)
build-max-heap(A) //run max-heapify on growing subtrees of A to create max heap         //O(n)
heapsort(A)       //build-max-heap, swap(A[0], A[length[A] - 1]), and max-heapify(A, 0) //O(nlog n)
*/

// 1 based indexing

void heapsort(int* A, int len);
void maxHeapify(int* A, int i, int len);
void buildMaxHeap(int* A, int len);
int parent(int i) { return i / 2; }
int lChild(int i) { return 2 * i; }
int rChild(int i) { return 2 * i + 1; }
int max(int A, int B);
void swap(int* A, int* B) { int t = *A; *A = *B; *B = t; }
void printData(int* A, int len) { for (int i = 1; i <= len; i++) { printf(" %d", A[i]); } printf("\n"); }
int elementAt(int* A, int i, int len);

int numbers[100000];
int main() {
  int Y; scanf("%d", &Y);
  while(Y--) {
  //---------------------------------
  
  int Z; scanf("%d", &Z);
  for(int i = 1; i <= Z; i++) {
    scanf("%d", &numbers[i]);
  }

  heapsort(numbers, Z);

  for(int i = 1; i <= Z; i++) {
    printf("%d ", numbers[i]);
  }
  printf("\n");
  
  //---------------------------------
  }
  return 0;
}

void heapsort(int* A, int len) {
  buildMaxHeap(A, len);

  for (int i = len; i >= 2; i--) {
    swap(&A[1], &A[i]); //element @1 is largest so stack it at the end
    maxHeapify(A, 1, i - 1); //restore shorter heap
  }
}

void buildMaxHeap(int* A, int len) {
  int lastNode = len / 2; 
  for (int i = lastNode; i >= 1; i--) {
    maxHeapify(A, i, len); 
  }
}

void maxHeapify(int* A, int i, int len) {
  if (i > len / 2) return; //we are in a leaf

  int lInd = lChild(i), rInd = rChild(i);
  int lVal = elementAt(A, lInd, len), rVal = elementAt(A, rInd, len);
  int largest = max(lVal, rVal);

  if (A[i] >= largest) return;
  if (lVal > rVal) {
    swap(&A[i], &A[lInd]);
    maxHeapify(A, lInd, len);
  } else {
    swap(&A[i], &A[rInd]); 
    maxHeapify(A, rInd, len);
  }
}

int max(int A, int B) {
  return A > B ? A : B;
}

int elementAt(int* A, int i, int len) {
  if (i > len) return -1;
  return A[i];
}
