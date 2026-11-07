---
phase: 2
title: "Sản xuất và tích hợp toàn campaign"
status: pending
priority: P1
dependencies: [1]
---

# Sản xuất và tích hợp toàn campaign

## Quy cách đã chốt khi thử trong game

- Toàn cảnh nhìn từ trên cao như bản đồ Arkham trong `arkham-gathering/dist/scene-art/arkham-city.png`. Công trình và địa hình phải phân biệt được trước khi đọc nhãn. Cảnh trong nhà dùng góc cắt lớp để thấy phòng, vách và cửa.
- Dùng tranh thẻ làm reference cho địa hình và kiến trúc. Các địa điểm cố định như làng dùng mốc trên tranh. Các phòng hoặc địa hình đổi vị trí dùng tranh mặt thẻ hiện hành ngay trong ô mà game đặt. Bảng brief bên dưới ghi chi tiết nhận diện của các location để đối chiếu nguồn.
- Nền toàn cảnh được dựng bằng imagegen từ ảnh thẻ CDN của game. Các ảnh thẻ theo ván dùng nguồn sẵn có của game. Mũi tên và lựa chọn di chuyển dùng kết nối và choice thực tế. First Evening và Second Evening hiện là đoạn kể chuyện, không tạo bàn chơi riêng.

## Brief theo bàn chơi

| Bàn | Ảnh gần | Toàn cảnh và trạng thái cần kiểm tra |
|---|---|---|
| Làng Hemlock Vale | Crossroads, Boarding House, Chapel, Atwood House, Old Mill, General Store, Schoolhouse, Commons | Một bố cục làng dùng chung cho Welcome, Dawn of the Second Day, Dawn of the Final Day và Final Evening. Ánh sáng sáng sớm/ban ngày/chiều lễ theo ngữ cảnh. Final Evening có thể bắt đầu ở Crossroads hoặc Boarding House. Chỉ vẽ node đã đặt. First và Second Evening dùng lại tranh làng nếu cần minh họa kể chuyện. |
| Written in Rock | Control Station, Rail Exit, bộ module Rail Tunnel theo hình đường ray, bộ hang Horrors in the Rock | Nền đường hầm cho phần đầu và nền lưới mỏ ở phần xe goòng. Ray rẽ, giao, cong, cầu và các mặt A/B là tile/cảnh theo mã. Xe goòng, vị trí và kết nối ray cập nhật từ ván. Kiểm tra Rail Exit và khác biệt standalone. |
| Hemlock House | Foyer, Bedroom, Washroom, Library, Parlor, Dining Room, Shapeless Cellar | Mở rộng pilot thành bộ phòng. Dùng cutaway nền trung tính, node theo lưới động. Dùng riêng biến thể có khác biệt thị giác quan trọng. Thử layout nở rộng, phòng bị loại, lửa và cellar xuất hiện. |
| The Silent Heath | Pearl Estate Ruins, Crystal Grove, Ashen Slope, Salt Chamber, Larval Tunnel, Crystal Nursery và các hang dùng chung | Mặt đất bạc màu với di tích. Phần ngầm có nền riêng hoặc lớp tối dần. Bố cục hang úp/ngẫu nhiên chỉ hiện theo game. |
| The Lost Sister | Akwan, bờ đá/bãi rong, vịnh ẩn, hang mở, hồ ngầm, nghĩa địa treo và hang nấm | Nền bờ biển và nền hang có thể mở rộng theo lưới. Cảnh sáng/tối dùng lớp ánh sáng khi đủ. Thử thứ tự hang khác nhau và Fungal Cave xuất hiện đúng lúc. |
| The Thing in the Depths | Bảy cảnh đầm lầy, trạng thái Open Water theo mặt b | Nền đầm lầy theo ô, lớp ngập hoặc ảnh Open Water dùng chung nếu hợp với bố cục. Thử thứ tự xáo trộn và nhiều ô lật liên tiếp. |
| The Twisted Hollow | Glimmering Woods, bộ rừng theo mã thẻ, The Twisted Hollow | Nền rừng đêm trung tính, lưới nở theo hướng mà game đặt. Giữ Hollow khỏi nền cho đến lúc xuất hiện. Standalone có điểm mở đầu khác. |
| The Longest Night | Farmhouse, barn, coop, milkhouse, pasture, vineyard và các biến thể Outer Fields | Toàn cảnh trang trại nhìn cao, Farmhouse là mốc cố định. Lớp phòng thủ, mồi, bẫy và diễn biến do UI vẽ. Kiểm tra các tổ hợp Outer Fields. |
| Fate of the Vale | Mirror Nest, The Abyss, địa điểm làng và hang được đặt lại | Nền không gian vỡ cho Mirror Nest và vùng Abyss. Dùng lại cảnh làng/hang với lớp biến dạng khi phù hợp. Dùng layout và liên kết thực tế khi rút và gắn địa điểm mới. |

## Thứ tự giao ảnh

1. Pilot Hemlock House → chốt ngôn ngữ hình ảnh, rồi hoàn thành bộ nhà và làng. Làm Atwood Farm sau làng để giữ nhất quán vật liệu và kiến trúc.
2. Làm bộ hang dùng lại và tile ray, rồi sản xuất Silent Heath, Lost Sister, Thing in the Depths và Twisted Hollow theo bảng. Làm Fate of the Vale sau khi bộ làng và hang đã sẵn sàng.
3. Đối chiếu [danh mục mã](./location-inventory.md) với setup, Act, Agenda và các effect gọi place/replace/flip để ghi rõ mã nào dùng ảnh riêng, dùng chung, hoặc chỉ hiện fallback ở trạng thái úp. Kiểm thử theo hai seed ở các màn có xáo vị trí và kiểm riêng các nhánh thay đổi topology.

## Tích hợp dự kiến

- `LocationScene.vue` tra ảnh theo mã thẻ + mặt/trạng thái nhìn thấy, dùng fallback mặt thẻ phù hợp khi thiếu ảnh. Dùng chung bố cục hotspot hiện có, thêm điểm đứng theo ảnh khi cần. Chỉ gắn điểm bấm thao tác khi có choice tương ứng.
- `LocationMap.vue` chọn nền theo scenario + trạng thái bàn, giữ preference chế độ xem của người chơi. `LocationMapCanvas.vue` nhận nền và dữ liệu vị trí. Chỉ đặt tọa độ thủ công cho mốc tĩnh đã xác minh, dùng vị trí lưới/đồ thị hiện có cho các ô động. Khi trạng thái đổi làm node biến mất, quay về location hiện hành của người chơi nếu có.
- Lưu asset runtime dưới `frontend/public/img/scenes/`, dùng thư mục campaign cho bộ mới. Chỉ tải nền và ảnh cần cho chế độ đang hiển thị. Bổ sung log chẩn đoán ở chế độ dev cho mã hoặc mặt thẻ thiếu ánh xạ.
- Chạy test frontend và typecheck theo `frontend/package.json`. Dùng save hoặc snapshot thật để kiểm tra đầu ván, sau Act/Agenda, reveal/flip, đặt location, di chuyển, undo/reload và ảnh fallback. Kiểm thử click, bàn phím, mobile và nhiều người. Đối chiếu move/đặt ô với choice từ server.

## Tiêu chí

- [ ] Hoàn thành bảng ánh xạ cho mọi location và mặt thẻ reachable, có ghi rõ asset dùng chung.
- [ ] Cảnh gần, toàn cảnh và Cards vẫn chọn được. Mỗi ảnh và hotspot khớp trạng thái game, kể cả khi chuyển Act/Agenda, reveal, flip hoặc dịch chuyển node.
- [ ] Không có lỗi tải asset trong đường chơi đã kiểm. Các fallback hoạt động khi cố tình bỏ một ảnh.
