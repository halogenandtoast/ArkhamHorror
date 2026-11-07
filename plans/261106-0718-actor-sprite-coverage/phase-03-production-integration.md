---
phase: 3
title: "Sản xuất và tích hợp"
status: pending
priority: P1
dependencies: [1, 2]
---

# Sản xuất và tích hợp

1. Hoàn thành Night of the Zealot sau lô thử, sau đó các investigator còn lại và từng campaign theo nhóm encounter. Theo dõi riêng weakness, special, biến hình và homebrew để kiểm đủ mã. Làm từng lô nhỏ, ghi lại prompt và reference, so từng kết quả với đúng lá bài trước khi nhập asset. Làm lại ảnh sai và giữ trạng thái thiếu nguồn cho đến khi xác minh được.
2. Đặt WebP đã duyệt trong `frontend/public/img/scenes/actors/`. Dùng mapping mã → file rõ ràng trong `frontend/src/arkham/sceneSprites.ts`, có alias được duyệt cho cùng artwork. Bỏ việc chọn Ghoul/Cultist bằng trait. Nếu có biến thể theo mặt hoặc form, truyền trạng thái art thực tế từ `LocationScene.vue`. Dùng fallback card art của chính entity khi sprite chưa có hoặc không tải được.
3. Chạy kiểm tra coverage đối chiếu roster với mapping, file, alpha, reference đã duyệt và alias. Thêm kiểm thử hồi quy cho hai Ghoul khác hình, Nightgaunt/Byakhee, bản in alias hợp lệ, mặt lật và mã chưa có sprite. Ghi debug có mã và đường dẫn khi lookup hoặc tải ảnh lỗi trong môi trường dev.
4. Chạy kiểm thử frontend và build. Kiểm browser ở The Gathering và các scene đại diện có enemy bay, enemy cỡ lớn, investigator biến hình, homebrew và nhiều figure cùng phòng. Kiểm tra responsive, trạng thái engaged và fallback. Xác minh URL WebP trong build triển khai. Cập nhật manifest nếu tuyến phục vụ asset thực tế yêu cầu.

## Nghiệm thu

- [ ] Mọi mã trong roster chốt truy xuất được sprite đã duyệt. Không có alias nhầm và không có file hỏng hoặc thiếu.
- [ ] So hình theo đúng card code ở các ca hồi quy. Scene tải được asset và giữ bố cục ở mobile lẫn desktop.

