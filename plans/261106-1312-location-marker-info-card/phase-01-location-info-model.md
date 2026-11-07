# Phase 1: Dữ liệu địa điểm và chặn spoiler

## Mục đích

Có một hàm thuần duy nhất biến `Location` + `Game` + choices thành model hiển thị. Cả bảng tên lẫn thẻ thông tin chỉ đọc từ model này. Mọi quy tắc spoiler nằm tại đây, nên không component nào phải tự quyết định nên lộ gì.

## Dữ liệu đã có (đã kiểm chứng)

- `Location`: `revealed`, `shroud` (giá trị in, decoder bỏ tag `PerPlayer`/`ValueX`), `tokens` (Clue, Doom, ...), `investigators`, `enemies`, `treacheries`, `assets`, `events`, `connectedLocations`, `keys`, `seals`, `floodLevel`, `brazier`, `breaches`, `modifiers`, `exhausted`.
- `public/cards_<lang>.json` có `shroud`, `clues`, `clues_fixed`, `victory`, `text`, `back_text`, `flavor`, `back_flavor`, `back_name`, `traits`, `back_traits`. Interface `ArkhamDBCard` ([dbCards.ts](../../frontend/src/stores/dbCards.ts)) còn thiếu `shroud`, `clues`, `clues_fixed` và `victory`.
- Với location, mặt "front" trong DB là mặt đã reveal. Mặt "back" là mặt chưa reveal.
- Move và Investigate là basic ability lấy location làm nguồn ([Location/Runner.hs](../../backend/arkham-api/library/Arkham/Location/Runner.hs), `AbilityMove` / `AbilityInvestigate`). Vì vậy `choicesFor(choices, l.id)` đã trả về "Move tới đây" cho phòng kề bên đi được, và "Investigate" cho phòng đang đứng.
- `CardOverlay.vue` có sẵn cách format text (`replaceText`, đọc `back_*` khi `needBack`). Nên dùng lại, không tự gọi `v-html` lên text thô.

## File

- Tạo `frontend/src/arkham/locationInfoModel.ts` cho hàm thuần `buildLocationInfo(...)` và kiểu `LocationInfo`. File **không được import gì**, giống `locationGraph.ts`: card DB, hàm đặt tên, format text, `choicesFor` đều truyền vào qua tham số. Như vậy test mới nạp được file mà không cần bộ build Vue.
- Tạo `frontend/src/arkham/composables/useLocationInfo.ts`, một composable mỏng nối model với store, i18n và choices.
- Tạo `frontend/tests/locationInfoModel.test.mjs` theo mẫu `tests/locationGraph.test.mjs` (`ts.transpile` rồi import qua data URL).
- Sửa `frontend/src/stores/dbCards.ts` để thêm các field tùy chọn `shroud?`, `clues?`, `clues_fixed?`, `victory?`.
- Sửa `frontend/src/arkham/types/Modifier.ts` để decode `ShroudModifier` và `SetShroud`. Phụ thuộc câu hỏi mở số 2.
- Nếu `replaceText` vẫn nằm trong `CardOverlay.vue` thì tách ra một helper dùng chung, ví dụ `frontend/src/arkham/cardTextFormat.ts`.

## Model

```ts
type LocationInfo = {
  id: string
  revealed: boolean
  name: string              // revealed ? name : (back_name ?? name)
  traits: string | null     // theo mặt đang lộ
  textHtml: string | null   // text mặt đang lộ, đã format icon
  flavorHtml: string | null
  artUrl: string            // cardImage(code, revealed ? '' : 'b')
  shroud: { printed: number; current: number } | null   // null khi chưa reveal
  clues: { left: number; printed: number | null } | null // printed = clues * số người nếu !clues_fixed
  doom: number
  victory: number | null    // chỉ khi revealed
  onEnter: boolean          // text mặt đang lộ có "Forced" + "enter"
  status: { here: boolean; adjacent: boolean; targetable: boolean; exhausted: boolean }
  people: { investigators: number; enemies: number }
  attached: string[]        // tên treachery/asset/event gắn vào location
  extras: string[]          // keys, seals, flood, brazier, breaches nếu có
  actions: { move: EntityChoice | null; investigate: EntityChoice | null; other: EntityChoice[]; selectIndex: number }
}
```

## Quy tắc chặn spoiler

1. Khi `!revealed`: tên lấy `back_name ?? name`. Text, flavor và traits lấy từ `back_*`. `shroud`, `clues.printed` và `victory` để null. `onEnter` chỉ tính trên `back_text`.
2. Không bao giờ đọc `text`/`flavor`/`victory` của mặt trước khi chưa reveal. Test phải khóa quy tắc này.
3. Kiểm tra alias `${code}b` trong dbCards trả về đúng `back_*` cho location có tên mặt sau khác mặt trước. Chọn một location kiểu đó trong data làm fixture.
4. Thông tin token (clue, doom) thì vẫn hiện, vì bàn chơi thật cũng nhìn thấy.

## Tính shroud hiện tại

- Nếu chọn hướng frontend: `SetShroud` thay hẳn giá trị. Cộng các `ShroudModifier` lại. Nếu có modifier "cannot be reduced" thì bỏ phần âm. Kết quả không nhỏ hơn 0. Khi `current !== printed` thì bảng tên đổi màu số để báo có modifier.
- Nếu chọn hướng backend: thêm field shroud đã tính vào JSON của location rồi đọc thẳng. Kiểu `PerPlayer` dùng `resolveCalculation` giống cách đã làm cho máu kẻ địch (ea0be77).

## Các bước

1. Thêm field vào `ArkhamDBCard`.
2. Tách helper format text nếu cần.
3. Viết test trước cho các ca: đã reveal / chưa reveal, back_name khác name, DB chưa tải, clue cố định và clue theo người chơi, shroud có modifier, phát hiện Forced on enter, phát hiện move và investigate trong choices.
4. Viết `buildLocationInfo` cho tới khi test qua.

## Kiểm tra

- `npm test` (chạy `node --test tests/*.test.mjs`)
- `npm run tc` (vue-tsc)
- `npm run lint`

## Rủi ro

- Text DB của location có thể dùng nhiều kiểu chữ khác nhau ("Forced -", "**Forced** –"). Bộ phát hiện `onEnter` nên dò rộng, và chỉ dùng kết quả làm gợi ý, không dùng để chặn.
- Location phiên bản encounter có thể dùng lại mã art (`cardArt`). Tra DB theo `cardCode` thật, không theo mã art.
