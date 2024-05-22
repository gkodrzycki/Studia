#include <bits/stdc++.h>

/*
Grzegorz Kodrzycki
332834
KPO
*/

using namespace std;

long long n, sum = 0;
long long dist[1000007];

int main(){
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n;
    
    for(int i = 0; i < n; i++){
        cin >> dist[i];
        sum += dist[i]; 
    }

    long long l = 0, r = 1, temp = dist[0], bestsofar = min(dist[0], sum - dist[0]), best = sum/2, d = dist[0];
    
    //max(min(temp, sum-temp),temp);
    while(true){
        
        if(d < best){
            d+=dist[r];
            r++;
        }
        else if(d > best){
            d -= dist[l];
            l++;
        }
        else{
            cout << bestsofar;
            return 0;
        }

        temp = d;
        temp = min(temp, sum-temp);

        bestsofar = max(temp, bestsofar);

        if(r == n){
            while(l != r){
                    temp = d;
                    temp = min(temp, sum-temp);
                    bestsofar = max(temp, bestsofar);
                    d -= dist[l];
                    l++;
            }
            break;
        }
    }

    cout << bestsofar;
    return 0;
}