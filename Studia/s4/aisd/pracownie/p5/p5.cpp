#include <bits/stdc++.h>

/*
Grzegorz Kodrzycki
332834
KPO
*/

using namespace std;

int board[1000001], parent[1000001], siz[1000001];
int n, m, T, ans;
priority_queue <pair <int, int>> vals;
vector <int> odp;
vector <int> t;


int find(int x){
    if(parent[x] == x)
        return x;

     if(parent[parent[x]] == -1){
        parent[x] = x;
        ans++;
        return x;
    }

    return parent[x] = find(parent[x]);
}

void uni(int x, int y){
        x = find(x);
        y = find(y);

        if(x != y)
            ans--;
        else
            return;

        if(siz[x] > siz[y]){
            parent[y] = x;
            siz[y] += siz[x];
        }
        else{
            parent[x] = y;
            siz[x] += siz[y];
        }
}

void solve(){
    vector<int> tocheck;
    for(int i = T - 1; i >= 0; i--){

        if(vals.empty()){
            odp.push_back(1);
        }
        else{
            if(vals.top().first <= t[i]){
                odp.push_back(ans);
            }
            else{
                while(!vals.empty() && vals.top().first > t[i]){
                    parent[vals.top().second] = vals.top().second;
                    siz[vals.top().second] = 1;
                    tocheck.push_back(vals.top().second);
                    vals.pop();
                    ans++;
                }

                for(auto e : tocheck){
                    // cout << e << "\n";
                    int fa = find(e);
                    int fb;
                    if(e%m != 0 && e - 1 >= 0 && board[e-1] > t[i]){
                        fb = find(e-1);
                        if(fa != fb){
                            uni(fa, fb);
                            // tocheck.push4 3_back(e-1);
                        }
                    }
                    if(e%m != m-1 && e + 1 < n*m && board[e+1] > t[i]){
                        fb = find(e+1);
                        if(fa != fb){
                            uni(fa, fb);
                            // tocheck.push_back(e+1);
                        }
                    }
                    if(e - m >= 0 && board[e-m] > t[i]){
                        fb = find(e-m);
                        if(fa != fb){
                            uni(fa, fb);
                            // tocheck.push_back(e-m);
                        }
                    }
                    if(e + m < n*m && board[e+m] > t[i]){
                        fb = find(e+m);
                        if(fa != fb){
                            uni(fa, fb);
                            // tocheck.push_back(e+m);
                        }
                    }
                }

                // cout <<"R\n";
                // for(int j = 0; j < m*n; j++){
                //     cout << parent[j] << " ";
                // }
                // cout << "\n";
                tocheck.clear();
                odp.push_back(ans);
            }
        }
    }
}


int main(){
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n >> m; 

    for(int i = 0; i < n*m; i++){
        cin >> board[i];
        vals.push({board[i], i});
    }

    for(int i = 0; i < n*m; i++){
        parent[i] = -1;
        siz[i] = 1;
    }

    cin >> T;

    for(int i = 0; i < T; i++){
        cin >> ans;
        t.push_back(ans);
    }
    
    ans = 0;
    solve();
    
    for(int i = odp.size() - 1; i >= 0; i--){
        cout << odp[i] << " ";
    }
    cout << "\n";
    return 0;
}