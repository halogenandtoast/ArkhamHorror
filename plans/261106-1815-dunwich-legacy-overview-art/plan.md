---
title: "Ảnh bản đồ toàn cảnh The Dunwich Legacy (Campaign 2)"
description: "Kế hoạch sản xuất tranh toàn cảnh và tích hợp Overview Map cho 8 scenario của Di sản Dunwich"
status: pending
priority: P1
created: 2026-11-06
---

# Kế hoạch sản xuất bản đồ toàn cảnh: The Dunwich Legacy (Season 2)

## 1. Mục tiêu & Phạm vi
- Sản xuất tranh toàn cảnh (Overview Map) chất lượng cao chuẩn 16:9 (1920×1080 WebP, ~250–350 KB) cho toàn bộ 8 scenario của campaign **The Dunwich Legacy** (gồm cả bản thường và Return to).
- Áp dụng triệt để quy chuẩn từ kỹ năng `arkham-overview-art`:
  - Loại bỏ hoàn toàn chế độ close-up; tập trung 100% vào trải nghiệm Bản đồ toàn cảnh (`map`) có tính năng tương tác node, plate, standee và đường nối.
  - Vị trí của các công trình/khu vực trên tranh **phải khớp 100% với ma trận bố cục không gian (`locationLayout`)** khai báo trong backend Haskell.
  - Phân loại rõ: **Mặt cắt kiến trúc nội thất (Cutaway)** cho các màn trong nhà và **Góc nhìn cao toàn cảnh (Isometric Bird's-Eye)** cho các màn ngoài trời/khu vực rộng lớn.

---

## 2. Danh mục 8 Scenario & Phân loại thị giác

| # | Scenario ID | Tên Scenario | Loại hình ảnh | Bố cục không gian (`locationLayout`) | Mô tả mốc địa lý chính cần vẽ |
|---|---|---|---|---|---|
| 1 | `02041` | **Extracurricular Activity** | Isometric Bird's-Eye | Lưới 3×4 (Khuôn viên ĐH Miskatonic trong đêm) | Thư viện Orne, Tòa nhà Nhân văn, Hội Sinh viên, Ký túc xá, Khoa Khoa học, Phòng Thí nghiệm Giả kim, Sân trường Quadrangle. |
| 2 | `02062` | **The House Always Wins** | Mặt cắt kiến trúc Cutaway | Mặt cắt hộp đêm Clover Club | Sảnh Lounge, Quầy Bar, Phòng Đánh bạc Card Room, Hành lang tối Darkened Hall, Khu VIP, và Hố ngầm Clover Club Pit. |
| 3 | `02118` | **The Miskatonic Museum** | Mặt cắt bảo tàng Cutaway | Hệ thống sảnh & phòng trưng bày | Cổng bảo tàng, Sảnh chính, Phòng cấm Restricted Hall, Phòng triển lãm Medusa Exhibit, Cổ vật Necronomicon. |
| 4 | `02159` | **The Essex County Express** | Mặt cắt đoàn tàu hỏa Cutaway | Hàng ngang nối tiếp các toa tàu | Toa đầu máy (Engine), Toa hành khách (Passenger Cars), Toa ăn (Dining Car), Toa hành lý (Baggage Car), Toa ngủ (Sleeper Car). |
| 5 | `02195` | **Blood on the Altar** | Isometric Bird's-Eye | Ngôi làng Dunwich cô lập u ám | Khu đất chung Village Commons, Suối Bishop's Brook, Tàn tích cháy rụi, Tiệm tạp hóa Osborn, Nhà thờ bỏ hoang, Ngôi nhà trong lau sậy. |
| 6 | `02236` | **Undimensioned and Unseen** | Isometric Bird's-Eye | Vùng đồng quê hoang tàn Dunwich | Cánh đồng hoang Blasted Heath, Khe núi Cold Spring Glen, Đồng cỏ mười mẫu, Bãi đất quỷ Devil's Hop Yard, Tàn tích nhà Whateley. |
| 7 | `02274` | **Where Doom Awaits** | Isometric Nghiêng dốc núi | Lối mòn leo núi Sentinel Hill | Chân đồi Base of the Hill, Lối mòn dốc Ascending Path, Ngã rẽ Divergent Path, Đỉnh tế lễ Sentinel Peak với bão sấm sét. |
| 8 | `02311` | **Lost in Time and Space** | Cosmic Trans-dimensional | Không gian đa chiều vũ trụ | Chiều không gian khác Another Dimension, Rìa vũ trụ, Vết rách thực tại, Cổng không gian, Thác nước lăng kính, Cầu vô tận Endless Bridge. |

---

## 3. Các giai đoạn triển khai (Phases)

### Giai đoạn 1: Khởi đầu tại Arkham (Scenario 1 & 2)
- **Extracurricular Activity (`02041`)**: Vẽ khuôn viên trường Đại học Miskatonic lúc đêm muộn với 8 tòa nhà được xếp chuẩn theo ma trận 3×4.
- **The House Always Wins (`02062`)**: Vẽ mặt cắt sang trọng pha lẫn tăm tối của sòng bạc chui Clover Club thập niên 1920.

### Giai đoạn 2: Điều tra & Đào tẩu (Scenario 3 & 4)
- **The Miskatonic Museum (`02118`)**: Vẽ mặt cắt bảo tàng lịch sử Miskatonic với không khí rùng rợn khi Hunting Horror rình rập.
- **The Essex County Express (`02159`)**: Vẽ mặt cắt ngang đoàn tàu hỏa hơi nước đang lao vùn vụt trong khi các vết rách không gian nuốt chửng từng toa từ phía sau.

### Giai đoạn 3: Ngôi làng Dunwich & Đỉnh Sentinel (Scenario 5, 6 & 7)
- **Blood on the Altar (`02195`)**: Toàn cảnh ngôi làng Dunwich đổ nát, bí ẩn, sương mù bao phủ các ngôi nhà gỗ mục nát.
- **Undimensioned and Unseen (`02236`)**: Toàn cảnh vùng đồi hoang vu nơi các thực thể vô hình của Yog-Sothoth tàn phá.
- **Where Doom Awaits (`02274`)**: Bức tranh đỉnh núi Sentinel Hill hiểm trở nhìn từ chân núi lên đỉnh tế lễ với vòng tròn đá cổ dưới bão sét.

### Giai đoạn 4: Hư không vũ trụ (Scenario 8) & Hoàn thiện
- **Lost in Time and Space (`02311`)**: Bức tranh vũ trụ kỳ vĩ, những khối hình học phi Euclid, cầu ánh sáng lơ lửng giữa hư không.
- **Tích hợp & Đăng ký Anchor:** Thêm toàn bộ 8 scenario vào `hemlock-overview.ts` và gán toạ độ neo cho các địa điểm để plate rơi chính xác vào tranh.
