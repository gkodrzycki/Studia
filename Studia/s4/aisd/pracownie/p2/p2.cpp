#include <bits/stdc++.h>

/*
Grzegorz Kodrzycki
332834
KPO
*/

using namespace std;

string s;
int w, k;
long long pot[10] = {1, 7, 49, 343, 2401, 16807, 117649, 823543, 5764801, 40353607};
long long sum = 0;


vector <vector<long long>> dp;
vector <vector<long long>> tab;
vector <long long> temp, zeros;



int main(){
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> w >> k;

    for(int i = 0; i < k; i++){
        zeros.push_back(0);
    }
    //uzupełnij pierwsze 2 warstwy
    for(int i = 0; i < 2; i++){
        cin >> s;
        temp.clear();
        for(int j = 0; j < s.size(); j++){
            temp.push_back(pot[int(s[j]) - '0']);
        }

        tab.push_back(temp);
        if(i == 0){
            dp.push_back(temp);
        }
        else{
            dp.push_back(zeros);
        }
    }

    for(int i = 2; i < w; i++){
        //Wczytaj nową linię
        cin >> s;
        temp.clear();
        for(int j = 0; j < s.size(); j++){
            temp.push_back(pot[int(s[j]) - '0']);
        }
        tab.push_back(temp);
        dp.push_back(zeros);

        //skok w dół i od razu skok w górę
       /* for(int K = 0; K < k; K++){
            if(K - 1 >= 0){
                dp[2][K-1] = max(dp[2][K-1], tab[2][K-1] + dp[0][K]);
                if(K - 3 >= 0){
                    dp[1][K-3] = max(dp[1][K-3], tab[1][K-3] + dp[2][K-1]);
                }
                if(K + 1 < k){
                    dp[1][K+1] = max(dp[1][K+1], tab[1][K+1] + dp[2][K-1]);
                }
            }
            if(K + 1 < k){
                dp[2][K+1] = max(dp[2][K+1], tab[2][K+1] + dp[0][K]);
                if(K + 3 < k){
                    dp[1][K+3] = max(dp[1][K+3], tab[1][K+3] + dp[2][K+1]);
                }
                if(K - 1 >= 0){
                    dp[1][K-1] = max(dp[1][K-1], tab[1][K-1] + dp[2][K+1]);
                }
            }
        }*/

        for(int K = 0; K < k; K++){
            if(dp[0][K] != 0){
                if(K - 1 >= 0){
                    dp[2][K-1] = max(dp[2][K-1], tab[2][K-1] + dp[0][K]);
                }
                if(K + 1 < k){
                    dp[2][K+1] = max(dp[2][K+1], tab[2][K+1] + dp[0][K]);
                }
            }
        }

        for(int K = 0; K < k; K++){
            if(dp[2][K] != 0){
                if(K - 2 >= 0){
                    dp[1][K-2] = max(dp[1][K-2], tab[1][K-2] + dp[2][K]);
                }
                if(K + 2 < k){
                    dp[1][K+2] = max(dp[1][K+2], tab[1][K+2] + dp[2][K]);
                }
            }
        }

        //usuń pierwszy wiersz tab & dp
        tab.erase(tab.begin());
        dp.erase(dp.begin());
    }

    for(int i = 0; i < k; i++){
        sum = max(sum, dp[1][i]);
    }

    cout << sum << "\n";
    
    return 0;
}