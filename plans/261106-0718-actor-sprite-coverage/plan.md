---
title: "Sprite cho toàn bộ investigator và enemy"
description: "Đối chiếu artwork theo mã thẻ và tạo sprite cho Location Scene"
status: pending
priority: P1
created: 2026-11-06
---

# Sprite cho toàn bộ investigator và enemy

Mỗi investigator và enemy có thể xuất hiện trong Location Scene của frontend LCG có sprite trong suốt cùng phong cách với Roland và Ghoul. Lấy danh mục từ phiên bản game trong repo, gồm campaign tích hợp, homebrew có sẵn, enemy weakness, enemy đặc biệt và các dạng nhân vật xuất hiện trong scene. Thẻ custom do người chơi thêm sau khi phát hành tiếp tục dùng art của chính thẻ qua fallback.

| Giai đoạn | Nội dung | Trạng thái |
|---|---|---|
| [1](./phase-01-start.md) | Chốt roster và ảnh reference theo từng mã | Pending |
| [2](./phase-02-reference-pilot.md) | Tạo lô thử và chốt chuẩn hình | Pending |
| [3](./phase-03-production-integration.md) | Tạo hết các lô, tích hợp và nghiệm thu | Pending |

## Hiện trạng

- `frontend/public/img/scenes/actors/` có tám WebP từ demo. Roland, Rats, Ghoul Priest, Umôrdhoth và Hunting Nightgaunt là các mapping có sẵn cần xác minh bằng ảnh gốc trước khi duyệt lại. Lita nằm ngoài roster nếu game render dưới dạng ally.
- `sceneSprites.ts` gán cả `01172` Hunting Nightgaunt và `01175` Screeching Byakhee vào `nightgaunt.webp`. Ghoul và Cultist dùng ảnh chung theo trait. Các lượt hiển thị đó chưa được tính là hoàn thành.
- `frontend/public/cards.json` có 100 dòng investigator và 609 dòng enemy. Đây là số dòng metadata, gồm các bản in và có thể thiếu mã nội bộ. Số sprite cần tạo được chốt ở giai đoạn 1.

## Nghiệm thu

- [ ] 100% mã trong roster đã chốt có sprite riêng hoặc mapping dùng lại đã xác nhận cùng artwork và cùng hình dạng.
- [ ] Mỗi sprite có mã thẻ, reference đã mở và đối chiếu, dấu vết duyệt và đường dẫn đầu ra. Không còn hồ sơ thiếu reference hoặc asset ở thời điểm nghiệm thu.
- [ ] Scene tải đúng WebP trên các cấu hình phục vụ ảnh đang hỗ trợ. Kiểm tra ảnh đúng ở kích thước chơi thực tế trên desktop và mobile.

## Tiến độ sản xuất

- Lô đầu đã tích hợp 32 WebP tạo từ mặt thẻ đúng mã, gồm Core Set và năm investigator Dunwich. Core Set còn dùng hai sprite demo Roland và Swarm of Rats đã đối chiếu. Ghoul Priest, Umôrdhoth và Hunting Nightgaunt có sprite mới theo thẻ.
- [Danh sách reference và SHA-256](./generated-references.csv) ghi nguồn của từng ảnh tạo. Những phần cơ thể nằm ngoài khung tranh gốc được dựng thêm để tạo standee toàn thân.
- Coverage hiện tại tính trên mã in trong metadata cho hai nhóm trên. Các bản in khác, các enemy Dunwich, các campaign còn lại, dạng nội bộ và homebrew vẫn cần kiểm kê và tạo ảnh theo roster runtime.

