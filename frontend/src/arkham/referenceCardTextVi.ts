/* Vietnamese text for the Feast of Hemlock Vale scenario reference cards (chaos-token effects).
 * The card database's Vietnamese file still carries the English text for these, so the mission strip
 * swaps these in when the interface language is Vietnamese. Game terms (token, resource, damage,
 * horror, doom, enemy keywords) stay in English like the rest of the Vietnamese card text. Side `a` is
 * the front (`text`), side `b` the back (`back_text`). */
export type ReferenceSides = { text: string; back_text: string }

const EASY = 'Dễ / Thường'
const HARD = 'Khó / Chuyên gia'

export const REFERENCE_TEXT_VI: Record<string, ReferenceSides> = {
  '10501': {
    text: `${EASY} - Act 1
[skull]: -X. X là số cột của địa điểm của bạn.
[cultist]: -1. Đặt 1 resource lên lá bài này, như một scrap.
[tablet]: -2. Nếu bạn thất bại, loại bỏ 1 scrap khỏi lá bài này.
[elder_thing]: -3. Nếu bạn thất bại, nhận 1 damage.`,
    back_text: `${EASY} - Act 2
[skull]: -X. X là số cột của địa điểm của bạn.
[cultist]: -2. Đặt 1 resource lên lá bài này, như một switch.
[tablet]: -3. Nếu bạn thất bại, di chuyển Mine Cart một lần.
[elder_thing]: -4. Nếu bạn thất bại, loại bỏ 1 switch khỏi lá bài này.`,
  },
  '10502': {
    text: `${HARD} - Act 1
[skull]: -X. X là gấp đôi số cột của địa điểm của bạn.
[cultist]: -3. Nếu bạn thành công, đặt 1 resource lên lá bài này, như một scrap.
[tablet]: -4. Loại bỏ 1 scrap khỏi lá bài này.
[elder_thing]: -5. Nhận 1 damage.`,
    back_text: `${HARD} - Act 2
[skull]: -X. X là gấp đôi số cột của địa điểm của bạn.
[cultist]: -3. Nếu bạn thành công, đặt 1 resource lên lá bài này, như một switch.
[tablet]: -4. Di chuyển Mine Cart một lần.
[elder_thing]: -5. Loại bỏ 1 switch khỏi lá bài này.`,
  },
  '10523': {
    text: `${EASY}
[skull]: -X. X là số tầng hiện tại của bạn (tối thiểu là 1).
[cultist]: -1. Nếu địa điểm của bạn là một kẻ địch, exhaust nó.
[tablet]: -3. Nếu bạn thất bại, heal 1 damage khỏi enemy-location gần nhất.
[elder_thing]: -4. Nếu bạn thất bại, và đang là agenda 2a hoặc 3a, sau khi bài kiểm tra này được giải quyết, loại bỏ token này khỏi chaos bag và thêm nó vào predation bag.`,
    back_text: `${HARD}
[skull]: -X. X là gấp đôi số tầng hiện tại của bạn (tối thiểu là 2).
[cultist]: -2. Nếu bạn thành công và địa điểm của bạn là một kẻ địch, exhaust nó.
[tablet]: -4. Nếu bạn thất bại, heal 1 [per_investigator] damage khỏi enemy-location gần nhất.
[elder_thing]: -5. Nếu đang là agenda 2a hoặc 3a, sau khi bài kiểm tra này được giải quyết, loại bỏ token này khỏi chaos bag và thêm nó vào predation bag.`,
  },
  '10549': {
    text: `${EASY}
[skull]: -X. X là một nửa số kẻ địch [[Insect]] đang in play và trong victory display (làm tròn lên).
[cultist]: -1. Bạn có thể xáo một kẻ địch không phải [[Elite]] trong victory display vào encounter deck.
[tablet]: -3. Nếu bạn thất bại, chọn một kẻ địch [[Insect]] tại địa điểm của bạn. Nó engage bạn và tấn công ngay lập tức.
[elder_thing]: -2. Nếu bạn đang ở một địa điểm [[Lair]], lật thêm một token khác.`,
    back_text: `${HARD}
[skull]: -X. X là số kẻ địch [[Insect]] đang in play và trong victory display.
[cultist]: -3. Nếu bạn thành công, bạn có thể xáo một kẻ địch không phải [[Elite]] trong victory display vào encounter deck.
[tablet]: -5. Ready kẻ địch [[Insect]] gần nhất. Nó di chuyển và tấn công như thể đang là enemy phase.
[elder_thing]: -4. Nếu bạn đang ở một địa điểm [[Lair]], bạn tự động thất bại.`,
  },
  '10588': {
    text: `${EASY}
[skull]: -X. X là một nửa số địa điểm [[Sunken]] đang in play (làm tròn lên).
[cultist]: -1. Bạn có thể loại bỏ 1 sinkhole khỏi địa điểm [[Bog]] gần nhất.
[tablet]: -3. Nếu bạn thất bại, đặt 1 damage lên địa điểm [[Bog]] gần nhất, như một sinkhole.
[elder_thing]: -4. Nếu bạn thất bại, tìm trong encounter deck và discard pile một kẻ địch Grasping Tendril và rút nó, bỏ qua keyword surge của nó.`,
    back_text: `${HARD}
[skull]: -X. X là số địa điểm [[Sunken]] đang in play (làm tròn lên).
[cultist]: -3. Nếu bạn thành công, bạn có thể loại bỏ 1 sinkhole khỏi địa điểm [[Bog]] gần nhất.
[tablet]: -5. Đặt 1 damage lên địa điểm [[Bog]] gần nhất, như một sinkhole.
[elder_thing]: -5. Tìm trong encounter deck và discard pile một kẻ địch Grasping Tendril và rút nó, bỏ qua keyword surge của nó.`,
  },
  '10605': {
    text: `${EASY}
[skull]: -X. X là mức darkness hiện tại.
[cultist]: -1. Heal 1 damage khỏi điều tra viên của bạn.
[tablet]: -3. Nếu bạn thất bại, tìm kẻ địch đang truy đuổi có evade cao nhất. Spawn nó tại địa điểm của bạn.
[elder_thing]: -4. Nếu bạn thất bại, nhận 1 horror.`,
    back_text: `${HARD}
[skull]: -X. X là gấp đôi mức darkness hiện tại.
[cultist]: -3. Nếu bạn thành công, heal 1 damage khỏi điều tra viên của bạn.
[tablet]: -4. Tìm kẻ địch đang truy đuổi có evade cao nhất. Spawn nó tại địa điểm của bạn.
[elder_thing]: -5. Nhận 1 horror.`,
  },
  '10626': {
    text: `${EASY}
[skull]: -X. X là một nửa số doom đang in play (làm tròn lên).
[cultist]: -2. Đặt 1 barrier tại địa điểm của bạn.
[tablet]: -1. Lật thêm một token khác. Nếu bạn thất bại, kẻ địch gần nhất ready, di chuyển và tấn công như thể đang là enemy phase.
[elder_thing]: -4. Nếu bạn thất bại, loại bỏ 1 barrier, decoy hoặc trap tại địa điểm của bạn.`,
    back_text: `${HARD}
[skull]: -X. X là số doom đang in play.
[cultist]: -3. Nếu bạn thành công, đặt 1 barrier tại địa điểm của bạn.
[tablet]: -4. Kẻ địch gần nhất ready, di chuyển và tấn công như thể đang là enemy phase.
[elder_thing]: -5. Loại bỏ 1 barrier, decoy hoặc trap tại địa điểm của bạn.`,
  },
  '10651': {
    text: `${EASY}
[skull]: -X. X là số act hiện tại cộng 1.
[cultist]: -4. Nếu bạn thất bại, discard ngẫu nhiên 1 lá từ tay bạn.
[tablet]: -5. Bạn có thể đặt 1 doom lên lá điều tra viên của bạn để coi modifier của token này là 0.
[elder_thing]: -2. Nếu đây là lần fight hoặc evade chống lại Cosmic Emissary, lật thêm một token khác.`,
    back_text: `${HARD}
[skull]: -X. X là số kẻ địch [[Colour]] đang in play.
[cultist]: -6. Discard ngẫu nhiên 1 lá từ tay bạn.
[tablet]: -8. Bạn có thể đặt 1 doom lên lá điều tra viên của bạn để coi modifier của token này là -3.
[elder_thing]: -5. Nếu đây là lần fight hoặc evade chống lại Cosmic Emissary, bạn tự động thất bại.`,
  },
  '10704': {
    text: `${EASY}
[skull]: -X. X là số ngày hiện tại.
[cultist]: +1. Nếu đây là một lần parley, bạn tự động thành công.
[tablet]: -2. Nếu bạn có ít hơn 3 lá trên tay, coi modifier của token này là 0.
[elder_thing]: -1. Nếu đây là một lần parley, lật thêm một token khác.`,
    back_text: `${HARD}
[skull]: -X. X là số ngày hiện tại cộng 1.
[cultist]: -1. Nếu đây là một lần parley, coi modifier của token này là +1.
[tablet]: -4. Nếu bạn có ít hơn 2 lá trên tay, coi modifier của token này là -2.
[elder_thing]: -3. Nếu đây là một lần parley, lật thêm một token khác.`,
  },
  '10569': {
    text: `${EASY}
[skull]: -X. X là một nửa số địa điểm [[Cave]] đã lật đang in play (làm tròn lên).
[cultist]: -1. Nếu bạn thành công, heal 1 horror.
[tablet]: -2. Nếu bạn đang ở một địa điểm [[Cave]], coi modifier của token này là -4.
[elder_thing]: -2. Nếu một kẻ địch [[Abomination]] ở tại địa điểm của bạn, lật thêm một token khác.`,
    back_text: `${HARD}
[skull]: -X. X là số địa điểm [[Cave]] đã lật đang in play.
[cultist]: -1. Nếu bạn thành công từ 2 trở lên, heal 1 horror.
[tablet]: -4. Nếu bạn đang ở một địa điểm [[Cave]], coi modifier của token này là -6.
[elder_thing]: -3. Nếu một kẻ địch [[Abomination]] ở tại địa điểm của bạn, bài kiểm tra này tự động thất bại.`,
  },
}
