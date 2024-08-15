#!/usr/bin/ruby -Ke

#                        Last modified: Sat Jan 10 12:04:19 JST 2004

# ���Υץ�����̵�ݾڤǤ���
# ���ѡ����ѡ������ۤ˺ݤ������¤Ϥ���ޤ���
## �¹Ԥˤ������줿����ʪ�˴ؤ��Ƥϡ����Ȥʤä�����˽��äƤ���������


# ����ˡ:
#   1. SKK-JISYO.(L|geo)�ʤɤ��Ф����Υץ�����Ŭ�Ѥ��ޤ���
#   2. ��̾��ʬ����䤬ʣ�������硢"XXX may be wrong."
#      �Ȥ��������Ȥ��ղä���Ƥ��ޤ���
#      �ܻ�ǥ����å�����Ŭ������ȥ��� or �����Ƚ���ޤ���
#   3. skkdic-expr2���� SKK�����μ���ˤ��ޤ���



# ���ԡ��衢������Į�ߤ����ʥ���ȥ��ʬ�䤷����硢
# ���̤ζ�ʬ�˾�̤ζ�ʬ�ξ�����ղä��롣
WITH_ANNOTATION = false

# �֤����롢�����롢�Ҥ������롢�ˤ�����פ����������ȥ����Ϥ��롣
## ���ʤߤˡ������Ͼ�פˡ֤�ʤ��ΤФ�Сס��֤�ʤ��ФСפ�����ɤߤ����ä���ȡ�
## geo����Υ١����ˤʤä��ǡ����ˤ����Τ��Ϥ���ۤɴ��ԤǤ��ʤ���
## ¾�ˤ�֤������󤰤��ɤ��꤫��Ϥ���礦�Ҥ������� /�ӿ����̲ϸ�Į����/�פȤ���
KYOTO_MODE = false

# ��̾�����ʬ�䤹��ˤ����äơ�
# �ɤߤ�ʬ��ѥ�����ʣ��������� HYOUKI2YOMI_DIC�ǡ�
# ɽ����ʬ��ѥ�����ʣ��������� NON_EXISTS �ǹʤ����Ȥ褤��
HYOUKI2YOMI_DIC = {
  '���ڻ�'   => '���äݤ�',
  '̾�Ų���' => '�ʤ��䤷',
  '���Ի�'   => '���礦�Ȥ�',
  '����'   => '����������',
  '���ͻ�'   => '�����٤�',
  '�����'   => '�Ҥ��ޤ�',
  '�̶彣��' => '�������夦���夦��',
  '�̷��Ϸ�' => '��������ޤ���',
  '���Ϸ�'   => '����ޤ���',
  '�ִ���'   => '���ܤ���',
  '��Ĵ�Ŷ�' => '������٤Ĥ�',
  '������'   => '���ĤϤ餯',
  '�����'   => '�����ޤ�'
}

NON_EXISTS = [
  '��', '��', '��', '��', '��', '����', 'Į', '�ޤ�', '���礦', '¼', '���', '����', '',
  # ���ϸ��ꡣ����ɬ�פ˱����ơ�
  '���ַ���', 'ȬƬ����'
]

# 0th: ʬ�䤹�븫�Ф�������ɽ��
# 1th: ���Ф���ʬ����Ѥ���ʸ��
# 2th: ʬ�䤹��ɽ��������ɽ��
# 3th: ɽ����ʬ����Ѥ���ʸ��
PATTERNS = [
  [/.+��.+��$/, '��', /.+��.+��$/, '��'],
  [/.+����.+(�ޤ�|���礦|���|����)$/, '����', /.+��.+(Į|¼)$/, '��'],
  # ������ʲ��ϡ�ñ�ȤΥѥ�����Τߤ�ͭ���ˤ������Ѥ����ۤ����������Ȼפ���
  # [/.+��.+/, '��', /.+��.+/, '��'], # �ֿ�����
  # [/.+�ɤ���.+/, '�ɤ���', /.+��.+/, '��'], # ���Υ롼��Ȳ��Υ롼��Ȥκ�ʬ�ˤϡ��äˤ�ܤ�������ȥ�Ϥʤ���
  # [/.+�ɤ���.+(������|������|�Ҥ�������|�ˤ�����)$/, '�ɤ���', /.+��.+/, '��'] # for kyoto-city
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

# split_timei_by_separator('����ޤ��󤰤�ޤ��礦')
#   => [['����ޤ���', '����ޤ��礦'], ['����ޤ��󤰤�', '�ޤ��礦']]

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
      # [['����������', '�Ҥ�����ɤ��勞'], ['�����������Ҥ���', '��ɤ��勞']]
      candidates.each{|candidate|
        if c_regexp =~ candidate
          data_c = split_timei_by_separator(candidate, c_sep)
          # [['����', '�������']]
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
        print yomi.sub(/(������|������|�Ҥ�������|�ˤ�����)$/, '')
        print ' /'
        print hyouki.sub(/(���|����|����|����)$/, '')
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
