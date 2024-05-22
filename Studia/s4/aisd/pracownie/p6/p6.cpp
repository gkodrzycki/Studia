#include <bits/stdc++.h>

/*
Grzegorz Kodrzycki
332834
KPO
*/

using namespace std;

int n, X1, Y1, X2, Y2, w, temp, maks = -1e9;
vector<int> X;
vector<int> Y;   
vector<pair<int, pair<int, int>>> start[200001];
vector<pair<int, pair<int, int>>> koniec[200001];
map<int,int> skalaX;
map<int,int> skalaY; 
const int BASE = (1<<17);
int tree[BASE<<1 + 5];
int maxy[BASE<<1 + 5];
bool more[BASE<<1 + 5];


void push(int x){
   more[2*x] = more[x];
   more[2*x + 1] = more[x];
   more[x] = 0;
   
   tree[2*x] += tree[x];
   tree[2*x + 1] += tree[x];

   maxy[x] += tree[x];
   tree[x] = 0;
}

int update(int a, int b, int x, int l, int r, int val){
    if(b < l || a > r){
        return tree[x] + maxy[x];
    }

    if(a <= l && r <= b){
        tree[x] += val;
        more[x] = 1;
        return tree[x]+maxy[x];
    }

    if(x < BASE && more[x]){
        push(x);
    }
    
    return maxy[x] = max(update(a, b, 2*x, l, (l+r)/2, val), update(a, b, 2*x+1, (l+r)/2 + 1, r, val));

}

int query(int a, int b, int x, int l, int r){
    if(a > r || b < l){
        return 0;
    }

    if(a <= l && r <= b){
        return tree[x] + maxy[x];
    }

    if(x < BASE && more[x]){
        push(x);
    }

    return max(query(a, b, 2*x, l, (l+r)/2), query(a, b, 2*x+1, (l+r)/2 + 1, r));
}

struct przedzial{
    int w;
    int X1;
    int X2;
    int Y1;
    int Y2;
};

przedzial przedzialy[100001];


int main(){
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n;
    
    for(int i = 0; i < n; i++){
        cin >> X1 >> Y1 >> X2 >> Y2 >> w;
        X.push_back(X1);
        X.push_back(X2);
        Y.push_back(Y1);
        Y.push_back(Y2);
        
        przedzialy[i].w = w;
        przedzialy[i].X1 = X1;
        przedzialy[i].X2 = X2;
        przedzialy[i].Y1 = Y1;
        przedzialy[i].Y2 = Y2;
    }

    sort(X.begin(), X.end());
    sort(Y.begin(), Y.end());
    
    int k = 0;
    for(auto e : X){
        if(skalaX.find(e) == skalaX.end()){
            skalaX.insert({e, k});
            k++;
        }
    }

    k = 0;
    for(auto e : Y){
        if(skalaY.find(e) == skalaY.end()){
            skalaY.insert({e, k});
            k++;
        }
    }


    for(int i = 0; i < n; i++){
        // cout << przedzialy[i].X1 << " " << skalaX[przedzialy[i].X1] << " " << przedzialy[i].X2 << " " << skalaX[przedzialy[i].X2] << "\n";
        start[skalaX[przedzialy[i].X1]].push_back({przedzialy[i].w, {przedzialy[i].Y1, przedzialy[i].Y2}});
        koniec[skalaX[przedzialy[i].X2]].push_back({przedzialy[i].w, {przedzialy[i].Y1, przedzialy[i].Y2}});
    }
    // cout << "END\n";

    // cout << "X " << X[0] << "\n";
    for(auto e : start[skalaX[X[0]]]){
        // cout << "ADD " << e.first << " " << e.second.first << " " << e.second.second << "\n";
        update(skalaY[e.second.first], skalaY[e.second.second],1,0,BASE-1,e.first);
    }

    for(int i = 1; i < X.size(); i++){
        if(X[i] != X[i-1]){
            // cout << "X " << X[i] << " " << start[skalaX[X[i]]].size() << " " << koniec[skalaX[X[i]]].size() << "\n";
            if(start[skalaX[X[i]]].size() != 0){
                for(auto e : start[skalaX[X[i]]]){
                    // cout << "ADD " << e.first << " " << e.second.first << " " << e.second.second << "\n";
                    update(skalaY[e.second.first], skalaY[e.second.second],1,0,BASE-1,e.first);
                }
            }
            
            if(koniec[skalaX[X[i]]].size() != 0){
                for(auto e : koniec[skalaX[X[i]]]){
                    // cout << "DEL " << e.first << " " << e.second.first << " " << e.second.second << "\n";
                    temp = query(0, BASE - 1,1,0,BASE - 1);
                    if(temp > maks){
                        maks = temp;
                    }
                    update(skalaY[e.second.first], skalaY[e.second.second],1,0,BASE-1, -e.first);
                }
            }
        }
    }

    cout << maks << "\n";
    return 0;
}