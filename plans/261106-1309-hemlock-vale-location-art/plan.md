---
title: "Ảnh địa điểm The Feast of Hemlock Vale"
description: "Kế hoạch mỹ thuật và tích hợp cảnh gần, toàn cảnh cho Arkham Horror LCG"
status: pending
priority: P1
created: 2026-11-06
---

# Ảnh địa điểm The Feast of Hemlock Vale

## Phạm vi

Lên kế hoạch cho cả 8 scenario và 6 prelude của campaign. Một lượt chơi campaign chỉ đi qua 6/8 scenario, nên danh mục sản xuất bao gồm cả tuyến bỏ qua và các setup standalone mà game hỗ trợ. First Evening và Second Evening là bước kể chuyện trong repo. Nếu cần minh họa hai đoạn này, dùng ảnh làng có sẵn trong bộ prelude. [Campaign Guide chính thức](https://images-cdn.fantasyflightgames.com/filer_public/5e/db/5edb4588-8583-4dc8-941c-26e1b7cfba56/ahc77_campaign_guide_web-compressed.pdf)

## Mẫu từ arkham-gathering

- `dist/house-map.js` chọn nền Study khi Study còn trên bàn, rồi chọn `house-cutaway` sau khi Study rời bàn. Node, cạnh và nhân vật được vẽ lên nền từ trạng thái game. Parlor vẫn bị khóa đến Act 3.
- `dist/scene-data.js` có cảnh riêng cho Study, Hallway, Attic, Cellar và Parlor, kèm tọa độ điểm bấm và điểm đứng. `dist/map-controller.js` theo dõi phòng đang xem và chuyển động kẻ địch. Chọn node mở chi tiết, còn di chuyển đi qua action hợp lệ.
- Repo hiện tại đã có ba chế độ Scene, Map và Cards. `LocationMap.vue` chỉ có cấu hình cutaway cho The Gathering, `LocationMapCanvas.vue` gắn đường dẫn ảnh nhà trực tiếp, còn `LocationScene.vue` có danh sách năm mã ảnh riêng rồi dùng card art cho địa điểm khác. Kế hoạch dùng lại các điểm tích hợp này và mở rộng theo scenario.

## Phases

| # | Giai đoạn | Trạng thái |
|---|---|---|
| 1 | [Chốt danh mục và thử Hemlock House](./phase-01-start.md) | Pending |
| 2 | [Sản xuất theo nhóm cảnh và tích hợp](./phase-02-production.md) | Pending |

Phase 2 dùng quy cách ảnh, quy tắc chọn nền và kết quả thử từ phase 1. Kế hoạch [port bản đồ The Gathering](../261106-0910-gathering-map-port/plan.md) là tài liệu tham chiếu của tính năng hiện có.

## Tiêu chí hoàn thành

- [ ] Mỗi location có thể xuất hiện trong cả 8 scenario và 4 prelude có bàn chơi được ánh xạ tới cảnh gần, gồm thẻ dự trữ, mặt biến đổi và các encounter set dùng lại.
- [ ] Mỗi bàn có nền toàn cảnh phù hợp theo trạng thái. Các cảnh một địa điểm dùng ảnh gần khi có ích, các bản đồ động bám bố cục của ván.
- [ ] Không lộ tên, mặt thẻ, căn phòng hay đích cuối trước khi game cho phép hiển thị. Từ mọi node, thao tác vẫn đi qua choice của server.
- [ ] Kiểm tra ảnh và lớp tương tác trên desktop, mobile, khi reload và trong ván có nhiều điều tra viên.

## Quyết định để thử trong pilot

Dùng tranh kinh dị thập niên 1920, nhất quán chất liệu giữa toàn cảnh và cảnh gần. Cho phép dùng chung ảnh giữa các mã nếu hình học và thông tin người chơi thấy giống nhau. Số ảnh gốc chốt sau pilot và kiểm tra các mặt thẻ. Hai đoạn kể chuyện có thể dùng lại tranh làng nếu sau này cần ảnh riêng cho màn kể chuyện.
