# Phase 2: Bảng tên địa điểm cho 3 kiểu vẽ

## Mục đích

Thay phần chữ trơn (`.title` + `.badges`) trong `LocationMapCanvas.vue` bằng một component bảng tên dùng chung. Bảng tên phải đọc được trên tranh tối, cho thấy phòng bấm được, và báo trạng thái chỉ trong một cái liếc.

## File

- Tạo `frontend/src/arkham/components/LocationMarkerPlate.vue`. Component nhận `info: LocationInfo` và `size: 'full' | 'compact' | 'mini'`, phát sự kiện `inspect` và `choose`.
- Sửa `frontend/src/arkham/components/LocationMapCanvas.vue` để dùng `LocationMarkerPlate` thay `.title`/`.badges`, và sửa luôn lỗi lộ tên ở dòng 110. File đang ở mức 199 dòng. Phần HUD của figure (`.fig`, `.hud`, `.meter`) nên tách thành `LocationStandeeFigure.vue` để file chính không phình thêm.
- Sửa `frontend/src/locales/en/base.json`, thêm các key `locationMap.marker.*`. Chỉ cần tiếng Anh, các ngôn ngữ khác tự rơi về tiếng Anh như các key hiện có.

## Thiết kế bảng tên

**Bố cục kích thước `full`** (cutaway và map dạng node):

- Dòng 1: icon trạng thái, tên địa điểm (Georgia, chữ hoa nhỏ), ⚠ nếu `onEnter`, nút ⓘ.
- Dòng 2, chỉ khi đã reveal: chip `🔍 shroud`, `◇ còn/in`, `☠ doom` (chỉ khi > 0), `👤 n`, `⚔ n` (chỉ khi > 0).
- Dòng 2 khi chưa reveal: chữ nghiêng "Chưa lật · vào để lật".
- Nền `#0f1712e6`, viền 1px vàng đồng `#c6a873`, bo 6px, đổ bóng `0 2px 10px #000c`, `backdrop-filter: blur(2px)` để tách khỏi tranh.

**Phân biệt trạng thái** (mỗi trạng thái dùng cả icon lẫn kiểu viền, không chỉ màu):

| Trạng thái | Icon | Viền/hiệu ứng |
|---|---|---|
| Đã reveal | ● | viền liền |
| Chưa reveal | ? | viền nét đứt, chữ nhạt hơn, nền sẫm hơn |
| Bạn đang ở đây | ★ | viền vàng sáng + glow `#ffe9a0` |
| Kề bên, Move được | ↔ | góc dưới có nút nhỏ "Đi tới" |
| Đang là target của choice | ✦ | viền vàng nhấp nháy chậm (tắt khi `prefers-reduced-motion`) |
| Exhausted | ⏸ | chữ xám |
| Shroud bị modifier | | số shroud tô màu cam hoặc xanh kèm tooltip "in: X" |

**Kích thước `compact`** (mini-map): chỉ icon trạng thái, tên rút gọn và `◇n`. Bỏ dòng 2.

**Kích thước `mini`** (màn ≤ 800px): giống compact nhưng vùng chạm tối thiểu 44×44px.

## Vị trí trên cutaway

- Bảng tên nằm ở **chân** cụm standee, giống vị trí chữ "Hallway" hiện tại. HUD máu và sanity vẫn ở trên đầu nhân vật.
- Thêm `z-index` theo `y` để phòng phía dưới đè lên phòng phía trên khi gần nhau. Kiểm tra Attic và Hallway trên ảnh 1672×941.
- Không gắn số tọa độ của The Gathering vào component. Component chỉ nhận vị trí từ `pos()`, để cutaway của Hemlock Vale sau này dùng lại được.

## Tương tác

- Click lên bảng tên: nếu `selectIndex !== -1` thì gửi choice đó (giữ nguyên hành vi chọn target), ngược lại phát `inspect`.
- Nút ⓘ luôn chỉ phát `inspect`, kể cả khi phòng đang là target. Đây là cách xem trước khi chọn (câu hỏi mở số 1).
- Nút "Đi tới" gửi `actions.move.index`. Không tự gửi thêm bước nào nữa.
- Bảng tên là `<button>` có `aria-label` gồm tên, trạng thái reveal, shroud, clue và "bạn ở đây". Ví dụ: "Attic, đã lật, shroud 1, 2 clue".

## Các bước

1. Tách `LocationStandeeFigure.vue` khỏi `LocationMapCanvas.vue`, hành vi giữ nguyên.
2. Viết `LocationMarkerPlate.vue` với 3 kích thước.
3. Nối vào canvas, bỏ `.title`/`.badges` và CSS cũ không còn dùng.
4. Sửa `activate()` trong `LocationMap.vue` để tách `inspect` khỏi `choose`.

## Kiểm tra

- `npm run tc`, `npm run lint`, `npm test`.
- Mở The Gathering bằng preview: kiểm tra bảng tên trên Study (1 phòng), trên cutaway (5 phòng), trên map dạng node và mini-map ở chế độ Scene.
- Chụp ảnh so sánh trước và sau, có cả trạng thái Attic chưa reveal và Hallway có kẻ địch.

## Rủi ro

- Bảng tên to có thể che standee. Giới hạn chiều rộng `max-width: 22cqw` và cho dòng 2 xuống dòng.
- Tranh sáng (Parlor màu đỏ) làm viền vàng kém nổi. Nền tối + blur xử lý được, cần nhìn tận mắt.
