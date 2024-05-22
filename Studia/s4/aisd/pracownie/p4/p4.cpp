#include <bits/stdc++.h>

/*
Grzegorz Kodrzycki
332834
KPO
*/

using namespace std;

bool F = false;
struct node
{
    long long val;
    struct node *left;
    struct node *right;
    int height;
};

node *root = NULL;

int height(node* t){
    if(t == NULL)
        return 0;
    return t->height;
}

node* rightRotate(node* t){
    node *x = t->left;
    node *y = x->right;

    x->right = t;
    t->left = y;
    t->height = max(height(t->left), height(t->right)) + 1;
    x->height = max(height(x->left), height(x->right)) + 1; 

    return x;
}

node* leftRotate(node* t){
    node *x = t->right;
    node *y = x->left;

    x->left = t;
    t->right = y;
    t->height = max(height(t->left), height(t->right)) + 1;
    x->height = max(height(x->left), height(x->right)) + 1; 

    return x;
}

int findBalance(node* t){
    if(t == NULL)
        return 0;
    return height(t->left) - height(t->right);
}

node* newNode(long long x){
    node *temp = new node; 
    temp->val = x;
    temp->left = temp->right = NULL;
    temp->height = 0;
    return temp;
}

node* insert(long long x, node* t){
    //Znajdz pozycję/zobacz czy istnieje
    if(t == NULL)
        return newNode(x);
    else if(x < t->val)
        t->left = insert(x, t->left);
    else if(x > t->val)
        t->right = insert(x, t->right);
    
    //Ogarnij drzewo
    t->height = max(height(t->left), height(t->right)) + 1;
    int balance = findBalance(t);
    if(balance > 1){
        if(x < t->left->val)
            return rightRotate(t);
        else if(x > t->left->val){
            t->left = leftRotate(t->left);
            return rightRotate(t);
        }
    }
    if(balance < -1){
        if(x > t->right->val)
            return leftRotate(t);
        else if(x < t->right->val){
            t->right = rightRotate(t->right);
            return leftRotate(t);
        }
    }
    return t;
}

node* findMin(node* t){
     if(t == NULL)
        return NULL;
    else if(t->left == NULL)
        return t;
    else
        return findMin(t->left);
}

node* remove(long long x, node* t){
    if(t == NULL)
        return NULL;
    if(t->val > x)
        t->left = remove(x, t->left);
    else if(t->val < x)
        t->right = remove(x, t->right); 
    else if(t->left && t->right){
        F = true;
        node* temp = findMin(t->right);
        t->val = temp->val;
        t->right = remove(t->val, t->right);
    }
    else{
        F = true;
        node* temp = t;
        if(t->left == NULL)
            t = t->right;
        else if(t->right == NULL)
            t = t->left;
        delete(temp);
    }
    

    if(t == NULL)
        return t;

    //Zbalansuj wariata
    t->height = max(height(t->left), height(t->right)) + 1;
    int balance = findBalance(t);
    if(balance > 1){
        if(findBalance(t->left) >= 0)
            return rightRotate(t);
        else{
            t->left = leftRotate(t->left);
            return rightRotate(t);
        }
    }
    if(balance < -1){
        if(findBalance(t->right) <= 0)
            return leftRotate(t);
        else{
            t->right = rightRotate(t->right);
            return leftRotate(t);
        }
    }
    return t;
} 

//Zwraca liczbę y ∈ P, taką że y <= x i y jest największą liczbą o takiej własności.
//Największy y mniejszy od x
long long lower(long long x, node* t, long long val){ 
    while(true){
        if(t == NULL)
            return val;
        
        if(t->val == x)
            return x;
        else if(t->val > x)
            t = t->left;
        else if(t->val < x){
            if(t->val > val)
                val = t->val;
            t = t->right;
        }
    }
}

//Najmniejszy y większy od x
long long upper(long long x, node* t, long long val){
    while(true){
        if(t == NULL)
            return val;
        
        if(t->val == x)
            return x;
        else if(t->val < x)
            t = t->right;
        else if(t->val > x){
            if(t->val < val)
                val = t->val;
            t = t->left;
        }
    }
}

void dfs(node* t){
    if(t == NULL)
        return;
    cout << t->val << " "; 
    if(t->left != NULL)
        cout << t->left->val << " ";
    else
        cout << "NULL ";
    if(t->right != NULL)
        cout << t->right->val << " ";
    else 
        cout << "NULL ";
    dfs(t->left);
    dfs(t->right);
}

long long n, a, b, m = -9223372036854775806;
char s;
int main(){
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    
    cin >> n;

    for(int i = 0; i < n; i++){
        cin >> s >> a;

        if(s == 'I'){
           root = insert(a, root);
        }
        else if(s == 'D'){
            root = remove(a, root);
            if(!F)
                cout << "BRAK\n";
            else 
                cout << "OK\n";
            F = false;
        }
        else if(s == 'L'){  
            b = lower(a, root, m);
            if(b == m )
                cout << "BRAK\n";
            else
                cout << b << "\n";
        }
        else if(s == 'U'){
            b = upper(a, root, -m);
            if(b == -m)
                cout << "BRAK\n";
            else
                cout << b << "\n";
        }

        dfs(root);
        cout << endl;
    }
    return 0;
}