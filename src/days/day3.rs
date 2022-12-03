use std::{collections::HashSet, ops::Deref};

pub fn day3() {
    let rucksacks: Vec<Rucksack> = INPUT
        .lines()
        .into_iter()
        .map(|line| {
            let compartments = line.split_at(line.len() / 2);
            Rucksack::from(compartments)
        })
        .collect();

    let total_priority = task1(&rucksacks);
    println!("Sum: {total_priority}");

    task2(&rucksacks);
}

fn task1(rucksacks: &Vec<Rucksack>) -> u64 {
    rucksacks
        .iter()
        .map(|r| *r.find_doubled_item() as u64)
        .sum::<u64>()
}

fn task2(rucksacks: &Vec<Rucksack>) {
    let mut badges = rucksacks
        .chunks(3)
        .map(|chunk| {
            let (a, b, c) = (&chunk[0], &chunk[1], &chunk[2]);
            a.common_items(b.common_items(c.all_items()))
        })
        .map(|set| {
            assert!(set.len() == 1);
            set.into_iter().next().unwrap()
        })
        .collect::<Vec<Item>>();

    badges.sort_by_key(|item| item.0);

    let badge_sum = badges.iter()
        .map(|item| **item as u32)
        .sum::<u32>();
        println!("Sum of Badge-Priority: {badge_sum}");
}

struct Rucksack {
    left: Compartment,
    right: Compartment,
}

impl Rucksack {
    fn find_doubled_item(&self) -> Item {
        let left = &self.left.items;
        let right = &self.right.items;

        let intersect = left.intersection(&right);
        intersect.last().unwrap().clone()
    }

    fn all_items(&self) -> HashSet<Item> {
        self.left.items.union(&self.right.items).cloned().collect()
    }

    fn common_items(&self, items: HashSet<Item>) -> HashSet<Item> {
        self.all_items().intersection(&items).cloned().collect()
    }
}

impl From<(&str, &str)> for Rucksack {
    fn from(compartments: (&str, &str)) -> Self {
        Rucksack {
            left: Compartment::new(compartments.0.to_string()),
            right: Compartment::new(compartments.1.to_string()),
        }
    }
}

struct Compartment {
    pub items: HashSet<Item>,
}

impl Compartment {
    fn new(items: String) -> Self {
        Self {
            items: items.chars().map(Item::from).collect(),
        }
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Item(pub u8);

impl Deref for Item {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<char> for Item {
    /// Creates item with priority
    fn from(item: char) -> Self {
        let priority = match item {
            'a'..='z' => (item as u8) - 96,
            'A'..='Z' => (item as u8) - 38,
            _ => unreachable!(),
        };
        Item(priority)
    }
}

#[cfg(test)]
mod tests {
    use crate::day3::*;
    use std::ops::RangeInclusive;

    #[test]
    fn test_item_first_last() {
        assert_eq!(1, Item::from('a').0 as usize);
        assert_eq!(27, Item::from('A').0 as usize);

        assert_eq!(26, Item::from('z').0 as usize);
        assert_eq!(52, Item::from('Z').0 as usize);
    }

    #[test]
    fn test_item_from_char_all() {
        let test_row = |chars: RangeInclusive<char>, start: usize| {
            for (index, c) in chars.enumerate() {
                assert_eq!(start + index, Item::from(c).0 as usize);
            }
        };

        test_row('a'..='z', 1);
        test_row('A'..='Z', 27);
    }
}

const INPUT: &str = "rNZNWvMZZmDDmwqNdZrWTqhJMhhgzggBhzBJBchQzzJJ
pHlSVbVbFHgHBzzhQHqg
nVsqGpbbtDtTNmrmfZ
zrBMnbzBchshsttfbMRBgmJggmmCHGgDhDgNDGHL
VddZqQqdvSQMJHJGdCDCDDmH
pZWWllPQlPZQvZvwpSVlqlvtfswMRzBbntzRbzbfstsRzF
NnjjRlnWNSWWbGwccbcchfPfTvfjfTBBpvmdMjTfvB
FVzJtDDJDqTMlmlM
gVQZlFLlzHhLGShGww
rPZtvtFrFPgWjQvCBlcqMzlqQC
QGVDJJnLnVTCJBczqqTM
fNSSnmLDSVLhhhSNSLhGSGfVPjrFHwmQwtwWFRWRjWPHrwgt
SvmlrVrCvmNhSSVZVCrsgqPfbwGFwwwsflbbGb
QHffdnHDDQdMGbgqPwztdPds
DjBjWHfQDfTQWTBfpMBQLVmmmcCCcVhCBBBhhCmC
trLHFFQHTLHJQrflfCnLLHrRfRRPqSRPbPbbsRGqqGqhjj
mcMpNWVVNmNVsSbSJPcGhPRR
NpzNgwzZDVNZVWNpHJQLQHtQrZQHrBCl
JVCMfgJVrJtMBhhrfVVfhVsjvpFGFgjSSgFdSGGqjvjvqF
mHllHlHpmWlDSFqbdSTS
nmZRLzQnWVpctMVpQs
BrvRzWBPWbRwGRjbbRGrtrfqjCJCjCJgJsZJscFCZcJC
MnnnVMVhTMQhsccVfwqFJgqf
mMShHHppQmHrrBzwtSbWwR
pWWGJMJJwlnZSqjWmvSWZC
gtHrLttDtgFjjqRZZCrjpp
bFtbTpHFHLbfLFbHVttccttddJGQdJzTwdTzJlMnMBwwJJ
JhqHFhVMzJPQcdcVncdc
NhgfwSjwCWwltSfnrnRWZdpcPrrRnp
NNhlltBjssNBgwLFFvDmDqLzHqBB
LnFrnddfrLnMFjWzpFhcWpjpFc
ntCwgtNggCqCgCqqPPltvcjjhvmWhmvDzTzDzD
lqlVQgVCSPVllVQSNGMHHrdQsHrJJBnMHHJf
ZGZcRZNWpcHZhJfbbNblrfrgllNr
stBMtzCCsHMfFQjfSSPgtt
qmszdsCzMncdGwdWZGvH
PccqPqbhvSvvvtWNjTtWsWcscp
gRwdDzHJQgHzfdRhgHRffzwsTTjTTCjNjssCpmWWDjtCLW
zdRMwdRHhGJwgHlnGGSFvvSrnSrr
rRpMJtPwrcCTNNQNMZQm
mDWdWVddbbbmBflFhvTHjjQjfZTgZgLLfH
bhBbFFnDVhdddFBhdmpJRrzStJmwnPzcsJ
RjlpRRWzzRGRmGzlCRRlQjCgtvTJTtJrTPttrWTwhFvvVJFT
bSBdLLqbcqcLndLHZNqcZdBDPrVTDDTJSFrJJvVthTwwDS
cqVsnBfHffVdqnZccGMmCsGzQmjsjlljgz
wMzJhLtwbnMWtHcFCCFqFNNbgq
fMlMfjrRRmdmGCGVVCHcVqcVTC
MmRRRlvmQWzpvnZpwJ
gRmgMRMmRwzzmwHbwcTNqPDVBbPTZVqPNZ
fWHphpGFpfJrrhPsNTNZVsNVhT
WGfJdvltJJfHrJpRgvMRMSwRznwMmw
htJFGsGspCppCFCGthCdpmJmgmWZfqqzWzlWcfgZHgzHlg
nwVMjVcVcWlbnBlfWB
wcNDTvPPDMFJLLppDGDD
hjCBgPbvMvmQDzlWnWjm
HrHtgZRRRNwczDWwwDzsQQWW
LpTqNtFtLFqHLHRrqgFHffVVBChvhhVPBCPhbPbp
CwpbCwjGqSjVllpGCllBfhZZRDPNcPPNvLLLDSDN
WshFFWsgTHsdMzQvPczLfLZDZRcLfR
rWsJQTMhWWHdsQTgsFJgllClVpqVbqnGblCppCVr
gRBSGcBDBSJSvPQwrTFLjggQTQ
HMMnHHHZfFVFrrMT
HhlhppCNcJzCTtBT
CCffCCmRLTsQRPHQQMPF
dWdbgcDSNclbbdwdSqHsvHPQPTPJplPMFMGJ
DWbDNcqZDSWSccNTVBCzVVfmBVZnVz
BnsrrvZwBsBSJrrrqSTgJQjCbCjgbCHDJgJFjQ
hLmGlnLmGWcjGDgfFFjQdF
hhWPmhPtczWpNRmppzRhLchMsnwZvTMZvVSwwrsNwSsBvr
tDCCltNVttJhNGlMPSWdqBqSjM
RFQcpcRTpFcnFzdLmLSWjMSSBLSQ
jwzzczpFbwnHcDCsthDJJsNbst
dLRWTHSwTmTwTcTWvQNVVQCvVvNFps
GnBPtBMJBPrjGGJMjrlqChNpNlsnhVFhQsVQ
JtMtGJfrJgDJjPjRTZLdFcRZRmwSDH
VSccPJSBLgZPDLDQ
zfpLMmLsHQGqgQHnDD
zdLLMssmrdfhddcVdJtScB
VvpTVQHSqSHSHqqHJVmRJVHpgDBwDgjcDDDgZjBZBjwBZbRw
PCdssGlstdWslFPfNPrtClGjwBgBJgJNwcjBjBgZwwMBJD
tlJldhdhdsdhTqSTqVQqQq
VGqTcTqbpPwrjfbl
BvntnZNNsLZvLszSnCsvJthlfjTrZwlrjrpPlwlhfwrl
QBtNtJLvTsFdQcqWmQRR
fjcjhmjBvcvcSvcZ
HMwZtRQQpGGRgzMvLnWWnbLlSntlbv
JQPzzJHqQRqGMMQwHwzDZZhmmPfjDjmjsCZhPj
cBlZZMfBrCBMwBMCvQzTwFbQzPnbwjTbTg
WtzpVDzmtthzGFQTbTThnnTQQg
sGWstpHdpGDmdHdmGmmmJNstRMrCcBSfBSzNBNRrSRNMcMMv
mMPDVBZZLSmRdcFpjr
fggGGfbfgQStjjsdbtdt
gNqQgCQlNCCJgJHvnvnHMjPHjv
bLsRQrQsGQbLrbRZMGgbJJBJFtlFFngJphhcfBBq
jjdHCCjfVNmmmNDFcBcpBthcplFDFq
jmvvmWVjjHTCVvNjSbQGLrRzwMWsMRwfGG
sJNCsCFFCNPhCzlrSvRrvwhRjj
MMGMTwpMHGzrGczzlG
qVmwgHtDtmCdWCsNFmNJ
fmhWhjVjNpqRRJjwRw
gnGQGDDCgSsCvPlvPgnPgnPtwqbpHRHqHdJpzpQJJJRJRF
wgPGsDGPsZgGgBmBWNZNfLWWrZ
WdsCVtjWWWHRRqLLHncC
fbSpMSPSZHRRcqlpRc
cGMmJmfMPPPccZMNQPWvjTtdTjvgmdtTsggw
tPBQhHWBtQHgWQCtLwddcGnfpGpwwnbhVb
vqQzTNJJJTvRrTNFJsZrrzFlbbfcnVbbcwmGGGpVzmddcdfd
NSSqJvFFFFFQjQCjQDSDPD
rQZnVVrZmZmgSWqHrSzHPC
LGFLwcMBcllBjFNwGjltggSqSWCCzvNgSqSHtt
wdhqqGBwwqGMcDhcwdFFbbJppZbssbfZQsQsdVQm
lqBZlsjVTbVqmFrSnTFSvwncPP
zQztHfZQtWLJzPFnnQScFcFrvS
ftHJWHhfttHWffhtgLNfZDWbdqBqjbVssBDCqCdCsmClGG
MlbWFTJQFbFFzRdNjNtjdtBT
srwnrsLVHzQPQsjjSQ
gLpnwgnwnHCvcHHcvwgCvGFFhWGmFmqMMbQFQFFhlGmJ
qqNcJgJccdqhsqgsggdgqgcrtfNWNZzVbvVFzttMfzbVMZ
GLlpPpCpwPLDGvrFVWrWWbZt
DlRCDDLSjTjDjSRSjPClwnwSHHHQmmQvTJcQgvddHsqdcgmB
jmRjRbRQLLZbPnbrcTTHHHNn
MfhhmmwtvStrpnJJHc
fgqlvfhvFzMwqfvMfFWlmMvLZsdQsZVdCdLZdGQjRzdQjD
lTPcDlVdTlVVMSDfTJccVzdlmMgGBmppgBmnHGHqHqQqqQMH
ZRjWFPsLNLLrPhWNtnBBvnpGpHGpQmHnmR
CtwssCNLrsZWjrjcbfPzwJJJffDbTl
cjMvvqpJFqhShNCRQR
ldtDgQZDPdzztLZgPTtfbnStfBSbNNSbnbhhSS
TDsrzsZZZTFHmVHjcsQW
BQmQchrmBddcmZZdpSgrpswWWswVsnnnDJVnnZFnGN
TfStMPLTHvbvRVGnHGsNnJWFNV
qtvMRMMPbbPMLqRPvRTRzMjSSmprpQdBchlmmgldgjzm
nRRnvNPhrbZDLjvS
HCszMwcHHcLDrbQDWr
ptszqwdMbnnhPBqN
QbzhhfbFhBbpbzwwLjLJjSjltL
mNndGrSStHJTJLln
rDMMNVWdVpCbSbSp
tDTSTSTTTTJDwqjWqBWttdjg
nNPmVfnGfPNVLmNzfnzPVFMjdpBwWZwZHwBLBqgjqpWH
dfGPfVQGVPhGzlmnzSvsSTDJhTbTTrrSRD
ZfgtZBptBfRQNQggjjrjjwmwsQJPzrwm
TwTGGwTwzzsJzTsH
lFvwqFLhFMnqcLlVLMLfptNWppppDBDbDfbFgW
mjftBfVPjttmjcSjcPttzJlvnrwvTRrTnvwvlRrHHTHRTR
WZDWDNLFWbZbcMDWGZDbNdMCRsnTdTvdnqrHCTrvsRRvwC
DQFZLNNgtBJQcBzJ
HbZQZFVbQVpQplQZGbGchDffltfLtmdgDjggTmtm
zWzRCdnCRBRdJrzDjLhDthjLJTTtjq
CPPnwSrRdRSzCGMcZZZMwFwMZF
WBQqNQnQllwnWQlvBBMlljHTqqFdGfmTdFfcFTFFcqmP
rsRRVrZhrzbtpZRRhFDmPvfFFrfTdFHGvc
VtSCtSLbtsZVtttthCbJSWSlJlwJQggWWglvwW
QfFLWCvRfSLFCtvtFhNcqDDcGVbhGcqh
ZVgrdZZPPZZzPwdjzZhmccsqJGqDdsDDNddD
pzzwpgZzZZTznZnjZZzPVRLQLlvfSlQRSpWlCvtSQv
RtcHhRMcrHhBrrTNDVBNLqLqQqfBPm
wCbWzWbvdWCjbWppmtmNmqmLLsfsNV
lwjWdbztgHTgggnnnR
flBbzbMfbrTlrMvBCcwPggdmcdmg
VDVVRFZRZSFFhQLSGFQhjSVZCgpvPwLCzpdWWzccwdvvvwcC
hDHRGQVHHQVRZSQGbqqfNTlbHzrbbsqb
MTFdTsZpPTcMpFCPdCBmMBmRfRGBmQgQRRgt
vbDSwvhzznnbbhDWnvSzRBgQQLgLQltqtqlmwfGB
jVjhfSnNDNbzzWzjWSjrCFNpcHdpTTJddJFpsJcc
ZrrZPHfChPdDPVVdDq
vFmsbTsmSbbBJssmSBvTmmnTrnrwlWqwVlLrVTLLTWqL
JrFbpsvFBMBmzBzFStcRhjZjfCCpZNCtct
TGgRrTggwwtvtQtdCdQNqN
sJHZJVZHDBpFBZBBNzNdhzdpSzddvqhN
VZcvFsJVFvsmvssbcnrwbrnGMbMlRn
SdcdWzMJdSMWMddZJdVcmBmwrwqrrnVnVNtr
mlQHCfgbjsfQTbfCBNtVhVnntVBnVh
HLDslDDmblgHfvLHPJFSZPpDFpFFpdPS
qNqPNJvcSzGGPQnGQp
bWhbgsshZWBhltthhbWtCsZNjrzpnQnnznnjtQFrjGjVFGnn
bRDNddhNdDsZdNChmvDmmwqqvLqwSJDq
TnSfPnCSmnSgpSTmfLzfMFLWFJJLWWsBsr
jdQjcdqDVVwDcPsPzMRJMLqPqR
PGhGchjhtZlTGTHCCb
ZZRrJJqSqJwNFFphsGsLPJ
blcMCflvTTPFFNpVvsFv
CcTlltTmtmMdmCmnlllBDDSDQSwSjRDQSdswjR
MCCPNsnQFWbvvTPF
CcCVJJhjVJZRtcCclDDlbcbTcGFFDz
HpjtVwVZfpjJVhZgCVtLmrBwdMrLsNNsMmdLqB
TJTDTnrFzzdWgWGJSSMJwg
LhPVttjtLmsPqqqVsVpsjLlgWlwHvGnlHWlgHlGgwvlP
mQshLhmsnsqZcqhZqpshsLVpNTNbBfzTRBQdFRzNNFBTdbzR
ZGqMLGqvJsJsMJmd
PDVQPfPcrrcFrrzrTdgCjSSCzgszmlJjBj
PfRtVfttVcWtVJrfbGqvwqLpRRwvpppH
HmLmMSnnWnrTrnvpqFCHVGfzVFVHQj
ttsstRhhcNwbswNtdwsdNPFfjzQppQPjfGGfQVPCpR
bbsDNtDcbhstsSZLDmSSgCmnSS
tfwBBLcJVrDnqvLv
zmWWJRZhWRRRGRNdgSZGgWTvpnjvrDqvpHjjzrpnrPDnHj
NdJmSGZWRhRNsghWTJmdGfQCtllCcFMwffBftsfMQc
lTLgTghpGZJDBrnGWnnm
VlRwlHttwqmHHbDWHJ
twldzCvsRdsFFtRtSczTjSgMcfSpSzTM
pBpMBTcSlNtMcTfFCmbPDzCDLb
JgrjjJqhGZQrQrZhnJGDDCZfvPDdDzFFdzfmZL
QHhqqnrVJJPhHrnGQgwMNwMMctcWRWSBMNtNsW
FJrlhpcfDCcFWpNpwWwjNQwz
RTTvPdbjWzMbnNNM
GRZTGggGgtvjGcqrBcttcDlFhr
pMRVdVbbMMMSdWWqHpCTvTjnBBBFFGGB
smNfZgcsNrcmzggZszsgRnPGFHjBPTBTjGjPTBNj
RmwgsmgfrzzsZtfgZLQQSVWlwbdMhlwdqQ
mRRjPmLrrSmzSczSzPgVZFpTCpZCMWrZQMQrZJZT
BvdbHNdnJtvBDbqqdBlvwvqpDQMpZQFMCsQCspZTMMCZCF
nBlfbfbndJBHPfLRfmhhhhPL
ScJDFBNLLbVRqVfZ
rWrgmdMgnnBhBtnntf
CwBWWMgCwddCgwsQjsrvNvlTJzSNHwNTHFJHzS
vnddCrNpCgtjLdSdgCgCCvLnWqDhWBQhHqQHDqBhQHDHNNDl
wPTVfVTJmZGJVJGffZBwHMWlWlHlWtbQDqbl
mGsJVVJsTVTTmtJVzzTJjdSjjprzCvpSLSCjdnLg
zLNggsVHmNNsssLmwzLQZLwDRvGQBqGGDDBBvvDBDqPhRG
WrCjbtJdbFhBRglGgjqv
JWCJcWcSdWcctnJCcJJJbcbmzwwznmgLzNzmLHmHZMwsZL
JRRDNNhhszMTzNMwCG
MnHPqmgmHjPnnvjqdmjFLQwLwTLwzTwTdGLCzS
BnPPZqmcfqgqnnZmBmqjqhfWVJlRMlhWlRDlVsssbh
nmTLTqsvqnwqsvwDPnLHdNVrMMHHCBlmVdmGNV
RgRpcJhQRfQZcJbWhQpBHCjVCdjCVGdddMllHp
fczbZhzbtcZfgRRBcWSPPwFsLSDswSwTsSzw
rbFpzFCVBrrBZCjbCzHHBVdJllGDLsLrDtsswswstGJs
QNhNNnNnnQhNWSnRhnJtdpJpJtMDGsGLLtsQ
ScmRvNRNnWWvNvNvfpTccjVZbqgZgVzqHjCjTVTVVq
BTppwCwBpwwBqnjlHcLBTHnbbSbDthsSSJgsnDDRgJRD
FVGzzvrdMGSSsdtZtZgd
QvQtvtGFlBLLjLQL
gsWWsNMjwgPMPWnMjShHHZSZbmZbbmTSnb
rlCvVQrCfqffpVjQRqCCvDDTTTmmZhZTmZhThFmhhZZhqb
CDDVJpVfrJJVJLMNzMwWwLwj
nHrcsZrssPcBPtQJLJtQQCZQpV
GFWzNzNFdNbTMMqbGTqTqzqqdLCpfDQCtRVVCLtdCfQsdCCt
TlNqGTWFNmMMszhGsmFTWGFzwHnvSjgPgvgSjllBvBnvwPBB
mpMggjgMlmtjtGMwZpcSscBlcsSblhsfSdfs
zzPVDRrLrCTQNCzNRTVFNLhBhBSqdQbcfSsJBJdbjJfB
RPTRPTVNTFzVrHVDCrTHmHtwMvwWMmtwmGjWgvGv
rLMcvfHVfMgLFvfNnBBzwRbBwnrGNs
dttJjJCtdjmwzwBCRRCqcs
TddDQDJDtQJtcJFpPQHPQMvfQlFL
LQSqqpqTCSJcsDcqQMMhnnjMjppZhwHZbZ
NRtvtmgmvdBffgtVCBWVRgFbPzHbMHbnwwjMPZfHbPjzPP
RNtvCvNdgtNNmldgvCFRNVLsQLqJcQGJJrccGSlDLDLr
GdwwqqqwGVtjdPvTCplbHTPbPzPTpp
RpLmLLpFfNsgTzclhzClThgH
ZFsWZLFZJsNsnWsnRsRfnfJQGBttjdGJjBvvwjdpjjttvj
tfPzzLrrdrQlTlvn
qJRBhNhNGVRBFRTlnJvCmvmJPCCl
VVPDNchNMVFGRMFcRVBjsZZcttSLSZzzStcWtZ
pTrwTrnjtttjprTSTNTQfcjcgPsPZfPgjdgdsQ
mCmCzvzhmJDHzJDbhFCDPsgddcsfcdsbdgVRpdVs
zqJzFCDhmqvGhMmCvmGhMCGJnSlnllSBLllLMtNpWtpNBnlt
JBhJrFLhGrnJZrlcbffndnggfggf
jqmWMGGSsqCsmpjmsDQzlcHgbtdzjjlVfctjHV
GWSmSCspCsMSpRmSmqMMCBvFLJLhTTwFhRFLLBTwrv
BCdWccqcqpQqrsNgGsWMgfNW
lFttLzzLwnfsLrsNsNLG
zjNlznlwvRPZnltwvPFnZRCbmjCcqjpcpQcqVVdbdVBm
CwTbbCGNFHtHwwjSjJpzjLMdMMzT
rscqqVvWgWrZMjrlmSzzmLrM
WPqqZnPqgncnBQQVRbCDwRHGSFHPwRNw
ZQnZwWjFvdsHwBJltfmfSlsqlJ
gPprhMDTpMpPMVNqNRqNlJhltJdJ
pLGCcCrgppCrVcMpdzjvzvjLwQQzFjwzHF
NmmmvfqcvmLSQhCLvtvL
TVlWTZVJZJsFbwWbQQhtQgLFCnSgghLt
hZJTJZhwZlRJrJWHVlblMBffmqfdNMjdGdBBqqcH
GJJfLfptGqqqnsVqVVjjDnNc
mZPSvPmBCdmwdCLDshSbRnnDDhRL
gvBrBvPBPPZCTLZmwmrgQdwfTJMHGzHfWffJzFzttHWFzW
sBMvmzWzmFmNWJfffZNLfbqZbtZq
jRQVRnhhppnVhjgnDLttLqbLqLQfDLss
jRRgpGVGhwhnspgpRppwSnBvMMcWvGczGJJHdmHJmJFF
VCLHFwHMhLghHHWhFFgWNMMVzmdmbvWdJqBPJPPBppqmBdzm
SRTsjGZTsZZnSnGZGqdBmrqPvmqqqsPpmv
GvQSGtZSQllVhtLMcLLNMH
GsNdWpdVWGSHjFCWCqFFgqngvW
mRQTcrLRmZTPRLPZfqqqHbDDDgFvFnvqzQ
hfZHrwwmcZRwlLfwlmrRjMJJsVjslVNBGNjpVBBG
pllpztRqBBvvGPpG
QQhhZQbVcZQTPMWWGbvvbMHM
cwgCQCLZChQwwLZVzCrzzqNCzrDqdFPF
bgcLPvvpcbdsbpSsHRTCqsRfWfsHRm
lZlQtthrnlVMmTHqqqqHSChB
rDtlzttnlSNrMtQjZVrcgGDLLddcdcpPgPGJJd
jvGbvLLQDSGlRmmSLjlDmRQggFBrMCwWdsBFWBFjdrrWrr
PpTfcPZpNTVNpHzTzzzpPJhBcwrrhFsrMdFcMCBFhgMF
JTTqdtfzfzJpqffNdTTHGtQRnmDQGGLQQlQRbblD
CQQCshCMwgQhMdjWJFBPpbjgmmWj
SNNvcGNSZSTDtGDcczJJBmzbjBJjmppbppms
cDtfDVNTGGGNNrwLLwHdqLhfLs
ngghZCChzhNjjNbbJfdh
slPPRLlBBlVRMvRllLLHvcpcdFfJjvdFpfHfcZ
RDZPZBLmPVWDVrQtnzSTmgTwmTSg";
