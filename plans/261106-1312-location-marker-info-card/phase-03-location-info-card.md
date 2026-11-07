# Phase 3: Thẻ thông tin và action nhanh

## Mục đích

Bấm ⓘ hoặc chọn một phòng thì mở một thẻ thông tin. Thẻ trả lời các câu hỏi: đây là đâu, đã lật chưa, vào thì bị gì, có gì ở đây, mình làm được gì.

## File

- Tạo `frontend/src/arkham/components/LocationInfoCard.vue`.
- Sửa `frontend/src/arkham/components/LocationMap.vue`: thêm state `inspectedId`, gắn thẻ vào `.map-layout`, đóng thẻ bằng Esc, click ra ngoài, hoặc khi phòng không còn trên bàn.
- Sửa `frontend/src/arkham/components/LocationScene.vue`: trong chế độ cận cảnh, nút "thông tin phòng" mở cùng thẻ này thay cho menu riêng.
- Sửa `frontend/src/locales/en/base.json`, thêm các key `locationMap.info.*`.

## Nội dung thẻ (từ trên xuống)

1. **Đầu thẻ**: ảnh thẻ thu nhỏ (`artUrl`, đúng mặt đang lộ), tên, traits, nhãn trạng thái "Đã lật" hoặc "Chưa lật", và "Bạn ở đây" hoặc "Kề bên" hoặc "Xa".
2. **Chỉ số** (chỉ khi đã lật): Shroud hiện tại (kèm "in: X" nếu khác), Clue còn/in, Doom, Victory. Thêm một dòng gợi ý điều tra: "Intellect của bạn 3 vs Shroud 2". Chỉ ghi so sánh số, không tính xác suất, vì kết quả còn phụ thuộc chaos bag.
3. **Khi vào / Hiệu ứng**: text mặt đang lộ, format icon. Nếu `onEnter` thì đóng khung cảnh báo ⚠ ở đầu khối. Chưa lật: hiện `back_text` (nếu có) và dòng "Lật khi có người bước vào". Tuyệt đối không hiện text mặt trước.
4. **Flavor**: chữ nghiêng, thu gọn mặc định, bấm để mở rộng.
5. **Ở đây có**: avatar điều tra viên, kẻ địch kèm máu, các treachery, asset hoặc event gắn vào phòng, và keys, seals, flood, brazier, breaches nếu có.
6. **Kết nối tới**: danh sách tên các phòng kề bên, theo đúng quy tắc tên của phase 1. Bấm vào tên thì chuyển thẻ sang phòng đó.
7. **Hành động** (tất cả lấy từ choices server đang đưa):
   - "Đi tới đây" ← `actions.move`
   - "Điều tra" ← `actions.investigate`
   - Ability khác của location ← `actions.other`, render bằng `AbilityButton` có sẵn
   - "Chọn nơi này" ← `selectIndex` khi đang có choice target
   - Nếu không có action nào thì hiện "Hiện không có hành động khả dụng ở đây". Không đoán lý do server không cho.

Mỗi nút gửi đúng `index` của choice. Sau khi gửi thì đóng thẻ, vì choices sẽ đổi.

## Bố cục

- Desktop: popover rộng 320px, neo cạnh bảng tên, tự lật sang trái hoặc phải để không đè panel Actions (rộng 300px bên phải) và cột Team (rộng 242px bên trái).
- ≤ 800px: bottom sheet chiếm 60% chiều cao, có nút đóng, cuộn trong sheet.
- Giữ nguyên `LocationActionPanel`. Thẻ thông tin chỉ chứa action của **một** phòng, còn panel vẫn liệt kê mọi action (câu hỏi mở số 3).

## Card DB chưa tải

Chỉ hiện tên (`nameOf`), token và action. Mục text, flavor và traits hiện skeleton. Khi store tải xong thì tự cập nhật vì model là computed.

## Các bước

1. Viết `LocationInfoCard.vue` đọc `LocationInfo`.
2. Nối `inspect` từ bảng tên và cận cảnh vào `inspectedId`.
3. Tính vị trí popover và bottom sheet.
4. Thêm phím Esc và trả focus về bảng tên khi đóng thẻ.

## Kiểm tra

- The Gathering: mở thẻ Attic khi chưa lật. Không được thấy "Forced – take 1 horror". Vào Attic rồi mở lại thẻ phải thấy cảnh báo ⚠ và shroud.
- Ở Hallway: thẻ Hallway có "Điều tra". Thẻ Cellar và Attic có "Đi tới đây". Bấm "Đi tới đây" thì server di chuyển đúng phòng.
- Khi có choice chọn location (ví dụ một hiệu ứng yêu cầu chọn location), bấm ⓘ không gửi choice, còn "Chọn nơi này" thì có.
- `npm run tc`, `npm run lint`, `npm test`.
