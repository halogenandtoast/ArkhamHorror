---
phase: 2
title: "Tạo lô thử"
status: pending
priority: P1
dependencies: [1]
---

# Tạo lô thử

Đường dẫn dưới đây đã đối chiếu với asset manifest của game. Mở từng ảnh và xác minh hình ở giai đoạn 1 trước khi đưa vào công cụ tạo ảnh.

| Mã | Nhân vật | Ảnh nguồn trong manifest | Việc cần làm |
|---|---|---|---|
| 01001 | Roland Banks | `img/arkham/cards/01001.avif` | Đối chiếu `roland.webp`, dùng làm mẫu dáng và ánh sáng |
| 01002 | Daisy Walker | `img/arkham/cards/01002.avif` | Tạo sprite |
| 01003 | “Skids” O’Toole | `img/arkham/cards/01003.avif` | Tạo sprite |
| 01004 | Agnes Baker | `img/arkham/cards/01004.avif` | Tạo sprite |
| 01005 | Wendy Adams | `img/arkham/cards/01005.avif` | Tạo sprite |
| 01160 | Ghoul Minion | `img/arkham/cards/01160.avif` | Xác minh `ghoul.webp` có đúng Ghoul Minion trước khi dùng lại |
| 01118 | Flesh-Eater | `img/arkham/cards/01118.avif` | Tạo sprite riêng |
| 01119 | Icy Ghoul | `img/arkham/cards/01119.avif` | Tạo sprite riêng |
| 01175 | Screeching Byakhee | `img/arkham/cards/01175.avif` | Tạo sprite riêng và thay mapping Nightgaunt |

1. Mỗi lượt tạo ảnh nhận crop artwork của đúng mã làm reference nhận dạng. Đính kèm Roland hoặc Ghoul làm mẫu phong cách với vai trò ghi riêng trong prompt. Với hình gốc cắt mất một phần cơ thể, giữ các chi tiết nhìn thấy và chỉ dựng tiếp phần ngoài khung.
2. Giữ khuôn mặt, tóc, trang phục, vật cầm và màu nhận diện của investigator. Giữ đầu, cấu trúc cơ thể, số chi, cánh và silhouette đặc trưng của enemy. Xuất một figure toàn thân hoặc trọn silhouette, có alpha, mép sạch, khoảng đệm và điểm chạm sàn phù hợp, bỏ khung và chữ của thẻ.
3. Làm bảng so sánh card/crop, sprite và ảnh chụp scene. So ở kích thước render hiện tại 110–210 px, thử nhân vật người đứng, quái dưới đất và quái bay. Chốt framing, cách nén WebP và giới hạn dung lượng dựa trên lô thử.

## Nghiệm thu

- [ ] Lô thử qua đối chiếu ảnh gốc và duyệt phong cách trước khi nhân rộng.
- [ ] Screeching Byakhee nhìn ra đúng loài khi đặt cạnh Hunting Nightgaunt. Flesh-Eater và Icy Ghoul phân biệt được ở kích thước trong scene.

