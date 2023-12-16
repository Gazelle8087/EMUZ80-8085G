# EMUZ80-8085G
アクセスいただきましてありがとうございます。

奥江聡さん作EMUZ80-Z8085のファームウエアを改造し  
2MHz動作においてROM読み込み、RAM書き込みを0Waitとしました。  

以前のファームウエアでは、合計6本の割り込みを使っていましたが  
(ALE, ROMread, RAMread, RAMwritem IOread, IOwrite)
ROMread割り込みをやめてALE割り込みルーチン内でROMread判定  
することにより、ROMread割り込みオーバーヘッド1回分250nsecを  
削減できました。  

RAM書き込み0waitの効果と合わせ2.5MHz 1wait(M1サイクル5クロック)よりも  
2.0MHz 0wait(M1サイクル4クロック)動作の方が高速動作となりました  

## 8085用コード  
8085用コードは以前作成したemuz80+mez8085用の物を流用しました。  
電源ONでモニタが立ち上がり、#1にてGrants Basicが起動します。  
モニタに戻るには BASICで MONITOR と入力ください。  
詳細は以下リンク先ご覧ください。  
https://drive.google.com/drive/folders/1abwFj7vFJUoVxqDc4fVLAgv0AT-kCIrY
## 変更履歴
2022.10.30 初版リリース  
2022.10.31 CLC記述のコメント誤記修正  
           （コードの変更はないのですが、読んだ方が惑わないよう）  
　　　　　　安定動作のため初期クロックを4.7MHzから4.0MHzに変更  
　　　　　 （手元の石の上限付近のまま公開してしまったので）  
2022.11.10 Aki.hさん作拡張版universal monitor を8080用にパッチ当て  
　　　　　　して組み込み。（ファーム本体は変更無し）  
2022.12.18 Aki.hさん作 EMUZ80 monitor Rev.B03 を8080パッチしマージ  
2022.12.29 Z80命令置き換えに間違いがあったのを修正  
			(SBC HL,BC と SBC HL,DE)  
2023.12.16 GitHub移行に合わせ、ROM READ 0 wait版を公開  

## 参照元や参考情報  
本ソースはGPLライセンス規約に基づき  
電脳伝説さん、奥江さんが公開されたソースを改変して作成いたしました。  

MEZ8085の詳細は以下リポジトリご参照ください  
https://github.com/satoshiokue/MEZ8085  

emuz80+mez8080用オリジナルファームウエアは以下リポジトリ参照ください。  
https://github.com/satoshiokue/EMUZ80-8085  

オリジナル・参考ソースへのリンクは、ソースの冒頭に  
リンクを記入していますので必要に応じご参照ください。  

本ソースとHEXはGPLライセンスを遵守のうえご自由に再配布いただいて構いません。  

2023.12.16 Gazelle https://twitter.com/Gazelle8087  
https://github.com/Gazelle8087/EMUZ80-8085G
