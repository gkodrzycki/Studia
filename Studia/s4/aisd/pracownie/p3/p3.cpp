#include <bits/stdc++.h>
/*
Grzegorz Kodrzycki
332834
KPO
*/

using namespace std;

int n, m, a, b, c, V, k, E, A, B;
int Vmask = 0xFFFFC000, Cmask = 0x00003FFF;

vector <int> v[100001];
priority_queue <pair <int, int>> q;
int dist[100001];
vector <int> F;

int main(){
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    
    cin >> n >> m >> k;
    

    dist[1] = 0;
    q.push({0, 1});
    for(int i = 2; i <= n; i++){
        dist[i] = 2147483647;
    }

    for(int i = 0; i < m; i++){
        cin >> a >> b >> c;
        A = a << 14;
        B = b << 14;
        A |= c;
        B |= c;
        v[a].push_back(B);
        v[b].push_back(A);
    }

    for(int i = 0; i < k; i++){
        cin >> a;
        F.push_back(a);
    }

    while(!q.empty()){
        V = q.top().second;
        c = -q.top().first;
        q.pop();
        
        if(c > dist[V]) continue;
        for(auto e : v[V]){ 
            E = e & Vmask;
            E = E >> 14;
            c = e & Cmask;
            if(dist[E] > dist[V] + c){
                dist[E] = dist[V] + c;
                q.push({-dist[E], E});
            }
        }
    }

    long long ans = 0;
    for(auto e : F){
        if(dist[e] == 2147483647){
            cout << "NIE\n";
            return 0;
        }
        ans += dist[e];
    }

    cout << 2*ans << "\n";
    return 0;
}