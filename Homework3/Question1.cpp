//Mausam Rayamajhi
//CSCI330: homework3
//Feb18th, 2025

#include <iostream>
#include <stack>
#include <vector>

using namespace std;

// Partitioning function
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Pivoting the last element
    int i = low - 1;       // Indexing the smaller element

    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) { 
            i++;
            swap(arr[i], arr[j]); // Swapping if the element is smaller than pivot
        }
    }
    swap(arr[i + 1], arr[high]); // Placing pivot in the correct position
    return (i + 1);
}

// Iterativing Quicksort
void quickSortIterative(vector<int>& arr) {
    stack<pair<int, int>> s;

    // Push initial low and high index to stack
    s.push({0, arr.size() - 1});

    while (!s.empty()) {
        auto [low, high] = s.top();
        s.pop();

        if (low < high) {
            int pivotIndex = partition(arr, low, high);

            // Push left sub-array (elements before pivot)
            if (pivotIndex - 1 > low)
                s.push({low, pivotIndex - 1});

            // Push right sub-array (elements after pivot)
            if (pivotIndex + 1 < high)
                s.push({pivotIndex + 1, high});
        }
    }
}

// Function to print the array
void printArray(const vector<int>& arr) {
    for (int num : arr)
        cout << num << " ";
    cout << endl;
}

// Main function
int main() {
    vector<int> arr = {10, 7, 8, 9, 1, 5, 3, 4};
    
    cout << "Original array: ";
    printArray(arr);
    
    quickSortIterative(arr);
    
    cout << "Sorted array: ";
    printArray(arr);
    
    return 0;
}
