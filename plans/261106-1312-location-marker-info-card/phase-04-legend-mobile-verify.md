# Phase 4: Chú giải, mobile, kiểm tra

## Mục đích

Giúp người mới đọc hiểu các icon, chạy ổn trên màn nhỏ, và kiểm tra toàn bộ trước khi gộp.

## File

- Tạo `frontend/src/arkham/components/LocationMapLegend.vue`: nút "?" trên `map-bar` mở bảng chú giải icon (●, ?, ★, ↔, ✦, ⚠, 🔍, ◇, ☠, 👤, ⚔). Trạng thái đóng hoặc mở lưu bằng `useStorage('arkham-map-legend')`. Lần đầu vào scene thì mở sẵn.
- Sửa `frontend/src/arkham/components/LocationMap.vue` để gắn nút chú giải.
- Cập nhật `docs/` chỉ khi có tài liệu mô tả giao diện map. Kiểm tra trước, chỉ sửa phần liên quan.

## Mobile

- Vùng chạm của bảng tên ≥ 44px. Chạm lần đầu mở bottom sheet thông tin (không có hover), còn chọn và di chuyển nằm trong sheet.
- Cutaway trên màn hẹp: bảng tên dùng kích thước `mini`, HUD standee giữ nguyên.

## Kiểm tra tổng

1. `npm test`, `npm run tc`, `npm run lint` đều qua.
2. Preview The Gathering theo thứ tự: Study một mình → mở nhà (cutaway) → di chuyển → điều tra → kẻ địch xuất hiện → reload trang.
3. Ván 1 và 4 điều tra viên: clue in nhân theo số người chơi, nhiều standee không đè bảng tên.
4. Một kịch bản không có cutaway (map dạng node, ví dụ Midnight Masks) và một kịch bản dùng grid. Bảng tên và thẻ thông tin vẫn đúng, ô grid trống vẫn bấm được.
5. Một location có tên mặt sau khác mặt trước: không lộ tên trước khi lật.
6. Màn 375px và 1440px. Thử `prefers-reduced-motion`.
7. Dùng code-reviewer cho diff cuối.
