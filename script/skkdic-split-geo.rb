#!/usr/bin/ruby -Ke

#                        Last modified: Sat Jan 10 12:04:19 JST 2004

# このプログラムは無保証です。
# 使用、改変、再配布に際して制限はありません。
## 実行により得られた生成物に関しては、元となった辞書に従ってください。


# 使用法:
#   1. SKK-JISYO.(L|geo)などに対しこのプログラムを適用します。
#   2. 地名の分割候補が複数ある場合、"XXX may be wrong."
#      というコメントが付加されています。
#      目視でチェックし、適宜エントリ削除 or コメント除去します。
#   3. skkdic-expr2等で SKK形式の辞書にします。



# 〜市〜区、〜郡〜町みたいなエントリを分割した場合、
# 下位の区分に上位の区分の情報を付加する。
WITH_ANNOTATION = false

# 「あがる、さがる、ひがしいる、にしいる」を除いたエントリも出力する。
## ちなみに、「柳馬場」に「やなぎのばんば」、「やなぎばば」と二つ読みがあったりと、
## geo辞書のベースになったデータには正確さはそれほど期待できない。
## 他にも「こうじんぐちどおりかわはらちょうひがしいる /荒神口通河原町東入/」とか。
KYOTO_MODE = false

# 地名辞書を分割するにあたって、
# 読みの分割パターンが複数ある場合は HYOUKI2YOMI_DICで、
# 表記の分割パターンが複数ある場合は NON_EXISTS で絞り込むとよい。
HYOUKI2YOMI_DIC = {
  '札幌市'   => 'さっぽろし',
  '名古屋市' => 'なごやし',
  '京都市'   => 'きょうとし',
  '大阪市'   => 'おおさかし',
  '神戸市'   => 'こうべし',
  '広島市'   => 'ひろしまし',
  '北九州市' => 'きたきゅうしゅうし',
  '北群馬郡' => 'きたぐんまぐん',
  '群馬郡'   => 'ぐんまぐん',
  '網干区'   => 'あぼしく',
  '音調津区' => 'おしらべつく',
  '勝原区'   => 'かつはらく',
  '飾磨区'   => 'しかまく'
}

NON_EXISTS = [
  '市', 'し', '区', 'く', '郡', 'ぐん', '町', 'まち', 'ちょう', '村', 'むら', 'そん', '',
  # ↑は固定。↓は必要に応じて。
  '日置郡郡', '八頭郡郡'
]

# 0th: 分割する見出しの正規表現
# 1th: 見出しの分割に用いる文字
# 2th: 分割する表記の正規表現
# 3th: 表記の分割に用いる文字
PATTERNS = [
  [/.+し.+く$/, 'し', /.+市.+区$/, '市'],
  [/.+ぐん.+(まち|ちょう|むら|そん)$/, 'ぐん', /.+郡.+(町|村)$/, '郡'],
  # これより以下は、単独のパターンのみを有効にして利用したほうが便利だと思う。
  # [/.+く.+/, 'く', /.+区.+/, '区'], # 「新区画」
  # [/.+どおり.+/, 'どおり', /.+通.+/, '通'], # このルールと下のルールとの差分には、特にめぼしいエントリはない。
  # [/.+どおり.+(あがる|さがる|ひがしいる|にしいる)$/, 'どおり', /.+通.+/, '通'] # for kyoto-city
]


def split_timei_by_separator(timei, sep)
  array = timei.split(sep, -1)
  retval = []
  for i in 1...array.size
    str1 = array[0...i].join(sep) + sep
    str2 = array[i..-1].join(sep)
    if !NON_EXISTS.include?(str1) && !NON_EXISTS.include?(str2)
      retval.push([str1, str2])
    end
  end
  retval
end

# split_timei_by_separator('ぐんまぐんぐんまちょう')
#   => [['ぐんまぐん', 'ぐんまちょう'], ['ぐんまぐんぐん', 'まちょう']]

def split_line_into_midasi_and_candidates(line)
  midasi, tmp = line.split(%r| /|, 2)
  candidates = tmp.split('/', -1)
  return midasi, candidates
end

def collect_entries(line)
  ret = []
  midasi, candidates = split_line_into_midasi_and_candidates(line)
  PATTERNS.each{|m_regexp, m_sep, c_regexp, c_sep|
    if m_regexp =~ midasi
      data_m = split_timei_by_separator(midasi, m_sep)
      # [['おおさかし', 'ひがしよどがわく'], ['おおさかしひがし', 'よどがわく']]
      candidates.each{|candidate|
        if c_regexp =~ candidate
          data_c = split_timei_by_separator(candidate, c_sep)
          # [['大阪市', '東淀川区']]
          data_c.each{|dc|
            for i in 0..1
              if HYOUKI2YOMI_DIC.has_key?(dc[i])
                data_m = data_m.select{|dm| dm[i] == HYOUKI2YOMI_DIC[dc[i]]}
              end
            end

            data_m.each{|dm|
              info = [ data_m.size > 1 ? midasi : nil, data_c.size > 1 ? candidate : nil]
              ret.push([dm[0], dc[0], nil, info])
              ret.push([dm[1], dc[1], dc[0], info])
            }
          }
        end
      }
    end
  }
  ret
end

while line = gets
  next unless line == ";; okuri-nasi entries.\n"
  break
end
while line = gets
  entries = collect_entries(line)
  for e in entries
    first = true
    loop{
      yomi, hyouki, annotation, info = e
      if KYOTO_MODE && first
        print yomi.sub(/(あがる|さがる|ひがしいる|にしいる)$/, '')
        print ' /'
        print hyouki.sub(/(上る|下る|東入|西入)$/, '')
      else
        print yomi, ' /', hyouki
      end
      print ';', annotation if WITH_ANNOTATION && annotation
      print '/'
      if info[0] || info[1]
        print '        XXX may be wrong. from '
        print info[0] || info[1]
      end
      print "\n"
      break unless KYOTO_MODE && first
      first = false
    }
  end
end
