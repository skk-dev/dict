type SKKJisyo = {
  meta: SKKMeta
  okuri_ari: SKKEntry[]
  okuri_nasi: SKKEntry[]
}
type SKKEntry = { [kana: string]: SKKHenkans }
type SKKHenkans = SKKHenkan[]
type SKKHenkan = { [kanji: string]: SKKAnnotations }
type SKKAnnotations = string[]

type SKKMeta = {
  description: string
  copyright: string
  license: string
  note: string
}