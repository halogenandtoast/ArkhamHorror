# Animation của chế độ bàn (cards) và những gì Scene còn thiếu

Ngày: 2026-11-06 · Nhánh: feat/location-map

## Kết luận

Hầu hết animation của chế độ bàn cần một phần tử DOM đích được đánh dấu (`data-card-flight`, `view-transition-name`, `data-investigator-mini`). Chỉ `Location.vue`, `Enemy.vue`, `Asset.vue`, `HandCard.vue` gắn các dấu này. Khi `settings.locationView !== 'cards'`, `Scenario.vue` render `LocationMap` thay cho các component đó, nên các animation này **lặng lẽ bị bỏ qua** trong Scene/Map. Không có lỗi hay treo bàn, chỉ là không có hiệu ứng.

## Cơ chế animation hiện có (chế độ bàn)

| Cơ chế | File | Điều kiện kích hoạt | Có trong Scene? |
|---|---|---|---|
| Bay thẻ (card flight) từ màn reveal tới chỗ thẻ nằm | `cardFlight.ts`, `Game.vue` (~556-790), `DrawSpotlight.vue` | View Transition API, phần tử có `data-card-flight` | Không: standee, plate, scene đều không gắn |
| Di chuyển investigator / enemy giữa các location | `investigatorMovement.ts`, `Game.vue` `entitiesMoved` (1320) | `startViewTransition` khi placement đổi, mini có `viewTransitionName` | Không: figure trên map không có tên |
| Smoke puff (bị loại khỏi game rồi quay lại) | `SmokePuffLayer.vue`, `smokePuff.ts` | Tìm `[data-investigator-mini]` | Không: chờ 600 ms rồi bỏ qua (lock được nhả trong `finally`, nên không treo) |
| Lật thẻ (act/agenda/location/enemy/asset) | `composables/useCardFlip.ts` | Ảnh mặt thẻ đổi | Không có trên plate / info card |
| Mưa, laser, flame wrap | `RainOverlay.vue`, `laserBeam.ts` (Connections), `flameWrap.ts` | Chỉ bọc khung location dạng thẻ | Không |
| Token damage/horror đang chờ, chaos token morph | `PendingDamageTokens.vue`, `ChaosTokenMorph.vue` | Gắn trên thẻ investigator/asset | Ở khu người chơi nên vẫn chạy; không hiện trên standee |

Scene hiện chỉ có: figure "thở", hotspot nhấp nháy, figure trượt `left/top` 0.7s trong một location (`LocationScene.vue`), plate `beckon` khi chọn được (`LocationMarkerPlate.vue`), chấm báo trên seat card.

## Đề xuất bổ sung cho Scene (xếp theo giá trị / công sức)

1. **Di chuyển giữa các location trên map.** Gắn `investigatorMovementName(...)` (và tên tương tự cho enemy) lên token figure trên `LocationMapCanvas`/`LocationStandees`. `entitiesMoved` đã tự mở view transition, nên figure sẽ trượt từ phòng này sang phòng kia mà không cần code animation mới. Lưu ý: trong `LocationScene` figure đã có CSS transition `left/top`; chỉ gắn tên ở lớp map, hoặc bỏ transition CSS, để tránh hai animation chồng nhau.
2. **Thẻ encounter bay vào đúng chỗ.** Gắn `CARD_FLIGHT_ATTR` + `useCardFlight(enemy.cardId)` lên standee enemy, và `useCardFlight(location.cardId)` lên plate location. Enemy spawn hoặc treachery gắn vào location sẽ bay từ màn reveal tới standee/plate, giống chế độ bàn.
3. **Smoke puff trên standee.** Thêm `data-investigator-mini="<id>"` và một phần tử `.portrait` (ảnh) vào figure investigator trong Scene. Layer hiện có sẽ tự dùng được.
4. **Phản hồi đòn đánh trên standee.** Enemy tấn công: standee lao tới (lunge) + rung investigator; số damage/horror bay lên trên đầu figure (dùng lại dữ liệu của `PendingDamageTokens`). Enemy bị đánh bại: mờ dần + rơi xuống. Cần tín hiệu: so sánh `damage`/`horror` giữa hai lần cập nhật game, hoặc một sự kiện UI như `arkham:smoke-puff`.
5. **Lật plate khi location được reveal.** Dùng `useCardFlip` trên `LocationMarkerPlate` / `LocationInfoCard`.
6. **Không khí.** Bọc khung Scene bằng `RainOverlay` cho những kịch bản đang bật mưa ở chế độ bàn; vẽ laser của Connections giữa các plate (tốn công hơn, giá trị thấp).

Nên làm 1 → 3 trước: chúng chỉ là gắn thuộc tính, tái dùng hạ tầng có sẵn và dễ kiểm tra.

**Cập nhật: đã làm 1 → 3.** Mỗi tên animation chỉ có một chủ, vì overview và cảnh cận (close-up) không bao giờ hiện cùng lúc:

| Hook | Overview (`LocationMapCanvas` + `LocationStandees`) | Cảnh cận (`LocationScene` + mini-map) |
|---|---|---|
| `investigator-<id>` (di chuyển) | figure trên standee / portrait trên map | portrait trên mini-map |
| `enemy-<id>` (di chuyển) | standee của enemy chưa engage | figure của enemy chưa engage trong phòng |
| `data-card-flight` enemy | ảnh standee | ảnh figure |
| `data-card-flight` location | plate (node hoặc room plate) | plate trên mini-map |
| Smoke puff (`data-investigator-mini` + `.portrait`) | standee / portrait trên map | figure investigator trong phòng |

Enemy đã engage không có tên di chuyển và không có đích card flight, vì lá enemy trong threat area (`Player.vue`) đang giữ các tên đó. Nhờ vậy, khi một enemy engage, nó sẽ bay từ phòng xuống threat area. `SmokePuffLayer.portraitFor` giờ chấp nhận trường hợp chính phần tử neo là portrait (ảnh trên map).

Kiểm tra: `vue-tsc` 0 lỗi; 8 test lỗi có từ trước và không liên quan; dev server biên dịch được mọi file đã sửa. **Chưa thử trong một ván thật.**

## Đã làm trong phiên này: xác nhận trước khi chơi thẻ / attack of opportunity

- `frontend/src/arkham/actionConfirmPolicy.ts` (+ `tests/actionConfirmPolicy.test.mjs`, 13 test pass) quyết định khi nào phải hỏi lại.
- `Game.vue` `choose()` là cửa duy nhất cho mọi lựa chọn (click, phím tắt draw/resource, kéo thẻ để chơi, hotspot Scene), nên nó hỏi lại trước khi gửi. Autopilot và chuỗi lựa chọn có thứ tự đi thẳng qua `submitChoice`. Nếu câu hỏi đã đổi trong lúc hộp thoại đang mở (ghế khác vừa hành động) thì câu trả lời bị huỷ, không gửi.
- Hai công tắc trong Settings, mặc định bật: "Confirm Card Plays" và "Warn on Attacks of Opportunity".
- Áp dụng cho **mọi chế độ xem**, không chỉ Scene, vì `choose()` dùng chung.

## Câu hỏi còn mở

- Có muốn làm ngay đề xuất 1 → 3 không?
- Server có thể có modifier miễn AoO mà client không thấy (ví dụ "ActionDoesNotCauseAttacksOfOpportunity"), nên hộp thoại ghi "may make an attack" chứ không khẳng định. Như vậy đã ổn chưa, hay muốn backend gửi cờ `provokes` chính xác cho từng lựa chọn?
- Chuỗi mới hiện chỉ có tiếng Anh; các ngôn ngữ khác dùng tiếng Anh làm dự phòng.
