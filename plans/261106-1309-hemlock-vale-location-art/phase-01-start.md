---
phase: 1
title: "Danh mục và pilot Hemlock House"
status: pending
priority: P1
dependencies: []
---

# Danh mục và pilot Hemlock House

## Đầu vào

[Dự án và tiêu chí](./plan.md) · [Danh mục location theo mã thẻ](./location-inventory.md)

## Quy cách ảnh

- Cảnh gần WebP, bản nháp 1600×900. Giữ phần dưới đủ thoáng để đặt người, quái và HUD. Tạo một ảnh theo địa điểm hoặc theo nhóm thực sự dùng chung, thêm biến thể khi lật mặt, đổi ánh sáng hoặc biến đổi cảnh có ý nghĩa.
- Toàn cảnh WebP, bản nháp 1920×1080. Nền tĩnh có mốc nhận diện theo địa điểm. Nền động dùng chất liệu môi trường và khoảng trống cho lưới hoặc đồ thị. Giữ cùng hệ tọa độ cho ảnh và overlay khi đổi kích thước. Thử nén đến khoảng 500 KB/cảnh gần và 900 KB/toàn cảnh, điều chỉnh nếu mất chi tiết tại điểm bấm.
- Tách nền khỏi tên, đường nối, token, trạng thái khóa, lửa, ngập và lựa chọn hành động. Toàn cảnh dùng góc nhìn cao hoặc cắt lớp. Cảnh gần giữ cùng kiến trúc, màu và hướng ánh sáng với mốc tương ứng.
- Mỗi brief ghi scenario ID, mã thẻ nguyên vẹn, mặt thẻ, trạng thái hiển thị, asset slug, loại ảnh, vật mốc, vùng đặt sprite và hotspot tùy chọn. Điểm bấm chuẩn hóa theo kích thước ảnh. Mã instance của game dùng cho node đang chạy.

## Pilot

1. Đối chiếu setup và các lần đặt, thay, lật hoặc bỏ phòng trong `Scenario/Scenarios/TheFeastOfHemlockVale/HemlockHouse.hs` và các Act liên quan. Chụp mẫu hai lối mở đầu Foyer và Bedroom.
2. Làm một cutaway nền khung nhà, hai cảnh mở đầu Foyer và Bedroom, cùng một cảnh Shapeless Cellar chỉ dùng khi phù hợp với trạng thái. Mẫu cutaway chỉ vẽ vỏ nhà, hành lang và tầng dưới ở mức trung tính. Để server quyết định phòng nào nằm ở ô nào, kể cả khi phòng mở rộng hoặc đổi vị trí.
3. Mở cảnh gần lúc bắt đầu nếu chỉ có một điểm điều tra đang hiển thị. Cho người chơi mở toàn cảnh khi bàn có thêm điểm. Giữ nút chuyển cảnh gần/toàn cảnh khi cả hai có ích. Dùng trạng thái setup và location thực tế thay vì giả định mọi Act 1 chỉ có một phòng.
4. Thử cùng một cảnh trên desktop và mobile. Kiểm tra node chọn đúng phòng, đường đi theo game, lối chưa mở không lộ, nhãn không đè lên actor, ảnh 404 vẫn có fallback.

## Tiêu chí

- [ ] Cả hai setup mở đầu Hemlock House có ảnh đúng.
- [ ] Cùng một nền nhà chạy được với ít nhất hai cách xếp phòng, kể cả một lần đặt phòng mới.
- [ ] Chốt brief, vị trí overlay và ngân sách ảnh từ mẫu chạy trong game trước khi sản xuất hàng loạt.

## Rủi ro

Nếu các ô động che mất hình chính, giản lược cutaway thành khung nhà và texture. Nếu ảnh cảnh gần lộ nội dung mặt sau, tạo ảnh trung tính hoặc dùng fallback mặt úp đến khi lật.
