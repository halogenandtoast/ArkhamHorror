# Tự động hoá giải quyết damage/horror (và các bước "không phải quyết định") kèm animation rõ ràng

Trạng thái: bản nháp, chờ duyệt. Nhánh: `feat/location-map`. Phạm vi: `frontend/` (LCG). Backend không đổi.
Nối tiếp [auto-flow autopilot](../261106-1353-auto-flow-autopilot/plan.md): autopilot đã có (`autoAnswerPolicy.ts`, `useAutopilot.ts`), kế hoạch này mở rộng allow-list và thêm lớp phản hồi hình ảnh.

## Kết quả mong muốn

Bị enemy đánh (hoặc treachery, hiệu ứng thẻ) thì damage/horror tự đổ lên investigator khi **không có lựa chọn thật**, và người chơi *thấy* điều đó xảy ra: số damage/horror bay lên khỏi figure, flash đỏ/tím, doom rơi lên agenda và nảy số. Chỉ dừng lại khi có quyết định thật (có asset soak, nhiều investigator, Ultimatum of Agony, v.v.).

Không làm: đổi luật, sửa Haskell engine, bot chơi hộ lượt, đụng `frontend-3ed`.

## Phát hiện chính (đã đọc source, chưa chạy ván thật)

Câu hỏi gán damage do `assignDamageDivided` (`backend/.../Investigator/Runner/Damage.hs` ~761-1081) đẩy ra có dạng:

`QuestionWithSource` → `QuestionLabel "Assign N damage [and M horror]"` → `ChooseOne [DamageLabel…, HorrorLabel…, AssetDamageLabel…]`

Engine đã tự "applyAll" khi chỉ có một đích, nên mỗi loại token chỉ tốn 1 click. Nhưng frontend vẫn bắt click vì hai lỗ hổng:

| # | Lỗ hổng | Chỗ | Hậu quả |
|---|---|---|---|
| 1 | `innerQuestion()` chỉ bóc `QuestionLabel` ở ngoài cùng; engine bọc thêm `QuestionWithSource` *ngoài* nó | `autoAnswerPolicy.ts` | `decideAutoAnswer` trả `null` cho **mọi** câu hỏi gán damage, kể cả khi chỉ còn 1 lựa chọn. (Cần test fixture để xác nhận.) |
| 2 | Vừa damage vừa horror, không có asset: có 2 lựa chọn (`DamageLabel iid`, `HorrorLabel iid`) | policy yêu cầu đúng 1 lựa chọn hợp lệ | Người chơi phải click Damage rồi Horror dù thứ tự không ảnh hưởng gì. |
| 3 | Không có phản hồi hình ảnh khi damage thật sự rơi | `PendingDamageTokens.vue` chỉ là bộ đếm tĩnh; Scene/standee không có hit-feedback (đã ghi ở scout `scout-261106-1427-scene-mode-animation-gaps.md` mục 4) | Nếu tự động hoá mà không có animation, mất thông tin. |

Bẫy cần tránh: lựa chọn chữa trị (Earthly Serenity, First Aid, Solemn Vow…) dùng **đúng cùng** `DamageLabel`/`HorrorLabel`. Phân biệt bằng `QuestionLabel` khớp `^Assign \d+ (damage|horror)` (cùng quy ước với `damageAssignmentTokens` trong `types/Game.ts` ~345). Không có label đó thì không tự trả lời.

## Phase

| # | Phase | Xong khi |
|---|---|---|
| 1 | Policy: tự gán damage/horror | Fixture đúng dạng engine; lone-choice và damage+horror (không asset) đều được chọn tự động; heal/asset/nhiều investigator vẫn dừng |
| 2 | Lớp phản hồi (FX) cho thay đổi trạng thái | Damage/horror/doom/clue/resource/enemy-defeat có hiệu ứng ở cả chế độ Scene lẫn Cards, tắt được, tôn trọng reduced-motion |
| 3 | Nối autopilot với FX | Autopilot chờ FX xong rồi mới trả lời bước kế; không có hai bước chồng nhau |
| 4 | Kiểm kê các điểm dừng khác | Bảng Ask thực tế (một vòng Gathering + một ván có asset soak) và danh sách ứng viên đã phân loại |
| 5 | Mở rộng allow-list từ bảng phase 4 | Mỗi shape mới có test; không có deny-list |

Phụ thuộc: 1 độc lập; 2 độc lập; 3 cần 1+2; 4 chạy song song; 5 cần 4.

### Phase 1: Policy tự gán damage/horror

File: `frontend/src/arkham/autoAnswerPolicy.ts`, `frontend/tests/autoAnswerPolicy.test.mjs`.

- Bóc nhiều lớp bọc (`QuestionWithSource`, `QuestionLabel`, `PayCostQuestion` nếu có) trong một vòng lặp, giữ lại `label` gặp được.
- Thêm lý do `assignDamage` vào `AutoAnswerReason` và `AUTO_ANSWER_DELAY_MS`.
- Điều kiện tự gán (tất cả phải đúng):
  1. label khớp `^Assign \d+ (damage|horror)`;
  2. mọi lựa chọn hợp lệ là `ComponentLabel` với `component.tag === 'InvestigatorComponent'`, cùng một `investigatorId`, `tokenType` ∈ {DamageToken, HorrorToken};
  3. không có `AssetComponent`, không có `AuxiliaryComponentLabel`, không có investigator khác.
  Chọn damage trước, horror sau. Vì phần còn lại của engine chạy lại câu hỏi cho phần còn lại, autopilot tự lặp.
- Giữ nguyên: có asset soak, nhiều investigator, Ultimatum of Agony thì dừng (đó là quyết định thật).
- Test (node, không cần Vue): fixture sao chép từ JSON thật của game; ca dương: 1 damage, 2 damage + 1 horror; ca âm: có `AssetComponent`, hai investigator, label heal, thiếu label.

### Phase 2: Lớp phản hồi (FX) khi trạng thái đổi

Mục tiêu: một nguồn sự thật cho "điều gì vừa xảy ra", để cả Scene lẫn Cards dùng chung.

- `frontend/src/arkham/composables/useGameDeltaFx.ts`: so sánh hai lần cập nhật `game` (đã có `entitiesMoved` trong `Game.vue` ~1320 làm mẫu), phát sự kiện: `damage` / `horror` (investigator), `enemyDamage`, `enemyDefeated`, `doom` (agenda, +/-), `clue`, `resource`. Mỗi sự kiện mang `targetKey` (investigator id, enemy id, agenda id) và số lượng.
- `frontend/src/arkham/components/FxLayer.vue`: vẽ hiệu ứng, neo theo `data-fx-target="<kind>-<id>"` (cùng tinh thần `data-card-flight` / `data-investigator-mini`). Mỗi hiệu ứng ngắn (400-900 ms), có hàng đợi để nhiều sự kiện không chồng nhau.

| Sự kiện | Hiệu ứng | Gắn vào |
|---|---|---|
| Investigator mất damage | Flash đỏ viền, figure rung, số `-N` đỏ bay lên, token damage từ pill "Tokens to assign" bay vào thanh máu | portrait/standee, `Investigator.vue`, `PendingDamageTokens.vue` |
| Investigator mất horror | Tương tự, màu tím, hơi mờ/nhoè | cùng đích |
| Enemy tấn công | Standee lao về phía investigator (lunge), rồi mới đến hiệu ứng damage/horror | standee enemy trong `LocationStandees` / `LocationScene` |
| Enemy trúng damage / bị hạ | Số `-N`; khi hạ: mờ dần và rơi xuống | standee enemy |
| Doom đặt lên agenda | Token doom rơi xuống pool, `PoolItem` nảy số `+N`, agenda rung nhẹ; khi chạm ngưỡng thì viền cháy | `Agenda.vue` (`PoolItem type="doom"`) |
| Clue/resource nhận hoặc mất | Token bay giữa pool và investigator, số nảy | `PoolItem` |

- Tách theo từng kích thước file (< 200 dòng/file): composable, layer, bảng định nghĩa hiệu ứng (data), CSS riêng.
- Cài đặt: công tắc "Combat/effect animations" (mặc định bật) trong `Settings.vue` + i18n; `prefers-reduced-motion` giảm còn flash/số, bỏ chuyển động.
- Hằng số thời lượng nằm một chỗ (`fxTimings.ts`) để phase 3 dùng.

### Phase 3: Nối autopilot với FX

- `useAutopilot` thêm tuỳ chọn `fxBusy: Ref<boolean>` vào `blocked` (đã có cơ chế `blocked`). Autopilot không trả lời khi FX đang chạy; FX phát từ cập nhật trạng thái, nên bước kế tiếp luôn đến sau khi người chơi đã thấy bước trước.
- `AUTO_ANSWER_DELAY_MS.assignDamage` ngắn (~250 ms) vì FX đã chiếm thời gian.
- Giữ nguyên các rào an toàn có sẵn: undo (`suspendUntilGameMoves`), chữ ký câu hỏi chống gửi đôi, giới hạn 60 lần liên tiếp. Thêm test: sau undo không tự gán lại ngay.

### Phase 4: Kiểm kê các điểm dừng khác

Chưa xác minh, chỉ là ứng viên từ việc đọc code; phải chạy ván thật để xác nhận trước khi tự động hoá:

| Ứng viên | Cách kiểm tra | Dự kiến |
|---|---|---|
| Nút "Continue/OK" lẻ sau khi enemy tấn công hoặc treachery resolve | ghi lại câu hỏi trong 1 vòng Gathering | tự trả lời nếu là lone `Label` không có `Info` |
| Hand size: số thẻ phải bỏ bằng đúng số thẻ trong tay | upkeep với tay quá đầy | tự bỏ khi không có lựa chọn |
| Enemy spawn / move có một đích hợp lệ | phase Enemy, hunter | engine đã tự; kiểm tra có còn dừng không |
| Thứ tự kích hoạt đồng thời mà kết quả giống nhau | nhiều forced effect | dừng, chỉ ghi nhận |
| Mulligan, chọn deck, commit thẻ | — | luôn là quyết định thật, không đụng |
| Gán damage cho enemy bị đánh (nhiều enemy engaged) | fight | quyết định thật |

Đây là phase 1 của plan autopilot nhưng cho riêng điểm dừng khi *bị tác động*; nên nhập kết quả vào bảng của plan đó thay vì làm bảng thứ hai.

### Phase 5: Mở rộng allow-list

Theo bảng phase 4, từng shape một: viết test trước (RED), thêm vào `decideAutoAnswer`, chạy lại. Quy tắc: allow-list, shape lạ trả `null`.

## Chấp nhận

- Bị đánh 2 damage + 1 horror, không asset: không cần click; thấy enemy lao tới, số damage/horror bay lên, thanh máu/lý trí tụt, rồi ván chạy tiếp.
- Có asset soak được: vẫn dừng và hỏi như cũ.
- Heal bằng Earthly Serenity / First Aid: vẫn hỏi như cũ.
- Doom đặt lên agenda luôn có hiệu ứng nhìn thấy, cả khi do hiệu ứng thẻ.
- Autopilot Off hoặc FX Off: hành vi hiện tại, không lỗi.
- Undo không bị tự gán lại ngay.
- `cd frontend && npm test`, `vue-tsc` sạch (tám test hỏng sẵn có từ trước không tính, theo báo cáo scout); chạy tay một ván Gathering.

## Rủi ro

- Nhầm quyết định thật thành tự động: allow-list + yêu cầu label "Assign…" + ràng buộc chỉ investigator của chính mình; undo vẫn còn.
- Mất thông tin ẩn khi tự động: FX là bắt buộc, không phải trang trí, vì thế phase 2 đi cùng phase 1, không đi sau.
- Diff trạng thái phát sự kiện giả (reload, đổi ghế, rewind): bỏ qua cập nhật có `step` lùi hoặc lần tải đầu; giới hạn hàng đợi.
- Hiệu năng ở Scene nhiều standee: hiệu ứng chỉ dùng `transform`/`opacity`, huỷ khi unmount.

## Câu hỏi còn mở

1. Có asset soak nhưng người chơi luôn muốn đỡ bằng asset: thêm tuỳ chọn "ưu tiên asset soak" (mặc định tắt) hay luôn hỏi?
2. `AuxiliaryComponentLabel` trong gán horror (`auxiliaryDamageInvestigator`) dùng cho trường hợp nào? Phase 1 đang coi là "dừng", cần xác nhận.
3. Có muốn animation chạy ở chế độ Cards ngay từ đầu, hay chỉ Scene/Map trước (nơi đang thiếu nhất)?
4. Mặc định thời lượng FX: nhanh (~400 ms) hay chậm, rõ (~900 ms)?
