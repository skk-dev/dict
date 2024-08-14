type SKKJisyo = {
  comments: string[]
  okuri_ari: Entry[]
  okuri_nasi: Entry[]
}
type Entry = [Kana, Kanji[]]
type Kana = string
type Kanji = [string, Annotations]
type Annotations = string[]
