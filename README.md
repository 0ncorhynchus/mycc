# mycc

[低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook)の習作。

## 進捗
- 電卓レベルの言語の作成
  - [x] ステップ1：整数1個をコンパイルする言語の作成
  - [x] ステップ2：加減算のできるコンパイラの作成
  - [x] ステップ3：トークナイザを導入
  - [x] ステップ4：エラーメッセージを改良
  - [x] 文法の記述方法と再帰下降構文解析
    - [x] 木構造による文法構造の表現
    - [x] 生成規則による文法の定義
    - [x] BNFによる生成規則の記述
    - [x] 単純な生成規則
    - [x] 生成規則による演算子の優先順位の表現
    - [x] 再帰を含む生成規則
    - [x] 再帰下降構文解析
  - [x] スタックマシン
    - [x] スタックマシンの概念
    - [x] スタックマシンへのコンパイル
    - [x] x86-64におけるスタックマシンの実現方法
  - [x] ステップ5：四則演算のできる言語の作成
  - [x] ステップ6：単項プラスと単項マイナス
  - [x] ステップ7: 比較演算子
    - [x] トークナイザの変更
    - [x] 新しい文法
    - [x] アセンブリコードの生成
- 分割コンパイルとリンク
  - [x] 分割コンパイルとは
    - [x] 分割コンパイルとその必要性
    - [x] ヘッダファイルの必要性とその内容
    - [x] リンクエラー
    - [x] グローバル変数の宣言と定義
  - [x] ステップ8: ファイル分割とMakefileの変更
    - [x] ファイルの分割
    - [x] Makefileの変更

- 関数とローカル変数
   - [x] ステップ9：1文字のローカル変数
     - [x] スタック上の変数領域
     - [x] トークナイザの変更
     - [x] パーサの変更
     - [x] 左辺値と右辺値
     - [x] 任意のアドレスから値をロードする方法
     - [x] コードジェネレータの変更
     - [x] メイン関数の変更
   - [x] ステップ10：複数文字のローカル変数
   - [x] ステップ11：return文
   - [ ] 1973年のCコンパイラ
   - [ ] ステップ12: 制御構文を足す
   - [ ] ステップ13: ブロック
   - [ ] ステップ14: 関数の呼び出しに対応する
   - [ ] ステップ15: 関数の定義に対応する
   - [ ] バイナリレベルのインターフェイス
- コンピュータにおける整数の表現
   - [ ] 符号なし整数
   - [ ] 符号あり整数
   - [ ] 符号拡張
   - [ ] 符号の反転
- ポインタと文字列リテラル
   - [ ] ステップ16: 単項&と単項*
   - [ ] ステップ17: 暗黙の変数定義を廃止して、intというキーワードを導入する
   - [ ] ステップ18: ポインタ型を導入する
     - [ ] ポインタを表す型を定義する
     - [ ] ポインタが指している値に代入する
   - [ ] ステップ19: ポインタの加算と減算を実装する
   - [ ] ステップ20: sizeof演算子
   - [ ] ステップ21: 配列を実装する
     - [ ] 配列型を定義する
     - [ ] 配列からポインタへの暗黙の型変換を実装する
   - [ ] ステップ22: 配列の添字を実装する
   - [ ] ステップ23: グローバル変数を実装する
   - [ ] ステップ24: 文字型を実装する
   - [ ] ステップ25: 文字列リテラルを実装する
   - [ ] ステップ26: 入力をファイルから読む
   - [ ] ステップ27: 行コメントとブロックコメント
   - [ ] ステップ28: テストをCで書き直す
- プログラムの実行イメージと初期化式
   - [ ] 実行ファイルの構造
   - [ ] データセグメントの内容
   - [ ] 初期化式の文法
   - [ ] グローバル変数の初期化式
   - [ ] ローカル変数の初期化式
- ダイナミックリンク
   - [ ] スタティックリンクとダイナミックリンク
- Cの型の構文
   - [ ] 型を表す図
   - [ ] 型を表す記法
   - [ ] Cの型の読み方
     - [ ] ネストしていない型の読み方
     - [ ] ネストしている型の読み方
   - [ ] 練習問題
