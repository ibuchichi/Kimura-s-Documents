############################################################
# 凝集してゆく日本区分図のクラスター色に連続性を持たせる処理
#
#  input:
#      ssdseN.cnum 凝集一つ前のクラスターベクトル(cutreeベース)
#      ssdseN1.cnum 凝集後のクラスターベクトル(cutreeベース)
#      ssdseNrenum.cnum 凝集前のクラスター番号振り直しベクトル
#
#  output:
#      ssdseN1renum.cnum 凝集後のクラスター番号振り直しベクトル
#
Cluster_Renumber_for_continuity <- function(ssdseN.cnum, ssdseN1.cnum, ssdseNrenum.cnum){

    # ssdseN.cnum から ssdseN1.cnum で凝集したクラスターの各先頭要素を確定する
    pref_name_back <- names(ssdseN.cnum[ssdseN.cnum != ssdseN1.cnum][1])
    back_cluster_num <- ssdseNrenum.cnum[pref_name_back] #凝集県の凝集前クラスター振り直し番号
    pref_name_forward <- names(ssdseN.cnum[ssdseN.cnum == ssdseN1.cnum[pref_name_back]][1])
    forward_cluster_num <- ssdseNrenum.cnum[pref_name_forward]　#凝集県の凝集前クラスター振り直し番号
  
    # 凝集する前のクラスターの要素数を確定する
    back_cluster_element_num <- length(ssdseN.cnum[ssdseN.cnum == ssdseN.cnum[pref_name_back]])
    forward_cluster_element_num <- length(ssdseN.cnum[ssdseN.cnum == ssdseN.cnum[pref_name_forward]])
    
    # 一つ前のクラスター番号振り直しベクトルを基に、クラスタ番号振り直しを行う
    if(forward_cluster_element_num >= back_cluster_element_num){
        ssdseNrenum.cnum[ssdseNrenum.cnum == back_cluster_num] <- forward_cluster_num
    }else{
        ssdseNrenum.cnum[ssdseNrenum.cnum == forward_cluster_num] <- back_cluster_num
    }

    ssdseN1renum.cnum <- ssdseNrenum.cnum
    
    return(ssdseN1renum.cnum)
}
