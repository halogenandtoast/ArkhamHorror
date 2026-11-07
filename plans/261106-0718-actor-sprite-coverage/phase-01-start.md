---
phase: 1
title: "Chốt roster và reference"
status: pending
priority: P1
dependencies: []
---

# Chốt roster và reference

1. Xuất roster từ các registry runtime trong `backend/arkham-api/library/Arkham/`: `Investigator/Cards.hs` (cả player và encounter), `Enemy/Types.hs` và `Enemy/Cards.hs` (encounter, weakness, special, homebrew). Đối chiếu code được tạo lúc chạy, biến thể in và dạng biến hình có thể xuất hiện trong `LocationScene.vue`. Xác định riêng các enemy-location được render như địa điểm để không tính nhầm thành figure.
2. Tạo bảng theo dõi với một dòng cho mỗi mã hoặc trạng thái hình có thể render. Ghi mã runtime, tên và subtitle, campaign/set, art code và mặt thẻ, file reference, hash reference, trạng thái, mã dùng lại nếu có, file WebP và người duyệt. Đối chiếu `cards.json` để bổ sung metadata.
3. Dùng `CardDef.art` và logic `cardFaceImages`/`cardImgPath` để tìm mặt đúng. Ưu tiên ảnh EN của chính lá thẻ từ asset local hoặc CDN của game, kiểm đường dẫn trong `frontend/image-manifest.json`. Artwork homebrew lấy từ nguồn của campaign. Portrait investigator chỉ bổ sung chi tiết nếu đã xác nhận cùng phiên bản. Thẻ lật hoặc biến hình dùng ảnh của mặt đang hiển thị.
4. Mở ảnh và so mã, tên, subtitle, set, mặt thẻ và đặc điểm hình. Lưu ảnh full card để đối chiếu và crop vùng minh họa làm input. Với enemy nội bộ như `xpolyp`, lần theo nguồn tạo enemy trong scenario để tìm đúng art. Nếu nguồn chưa đủ căn cứ, đánh dấu thiếu reference và ghi chính xác cần bổ sung gì.
5. So ảnh cũ với từng reference trước khi đánh dấu dùng lại. Hai mã chỉ dùng chung khi đối chiếu được cùng minh họa và cùng hình dạng. Tên, trait và nhóm bản in chỉ là dữ liệu để tìm ứng viên.

## Nghiệm thu

- [ ] Có tổng mã runtime, tổng artwork riêng, số ảnh cũ dùng lại, số cần tạo và danh sách mã thiếu nguồn.
- [ ] Mọi mã trong lô thử có ảnh reference đã mở và khớp đúng mặt. Bảng ghi rõ mã nào được dùng lại và căn cứ.

