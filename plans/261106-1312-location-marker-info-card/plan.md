---
title: "Bảng tên địa điểm và thẻ thông tin trên scene"
description: "Làm đẹp nút địa điểm, hiện rõ trạng thái reveal, shroud, clue, hiệu ứng khi vào và action có thể làm"
status: in-progress
priority: P1
created: 2026-11-06
branch: feat/location-map
---

# Bảng tên địa điểm và thẻ thông tin trên scene

## Vấn đề hiện tại

Ảnh chụp toàn cảnh nhà The Gathering cho thấy:

- Tên phòng là chữ trơn nằm trên tranh ("Hallway", "Parlor", "Cellar"). Chỉ phòng đang chọn có viền ("Attic"). Chữ dễ chìm vào nền, và nhìn vào không biết đâu là nút bấm được.
- Ở chế độ cutaway, `.cutaway .art` và `.node::after` bị ẩn, nên không còn dấu hiệu nào cho biết phòng đã reveal hay chưa. Clue và doom chỉ hiện khi khác 0. Shroud không hiện ở đâu cả.
- Người chơi phải mở Card board để đọc mặt thẻ. Từ map không biết vào phòng sẽ bị gì (Attic: Forced, nhận 1 horror. Cellar: nhận 1 damage), cũng không biết làm được gì ở đó.
- Bấm vào một node đang là target sẽ gửi choice ngay trong `activate()` ([LocationMap.vue:310](../../frontend/src/arkham/components/LocationMap.vue)). Vì vậy không có cách nào xem thông tin một phòng trước khi chọn nó.
- Lỗi lộ spoiler sẵn có: [LocationMapCanvas.vue:110](../../frontend/src/arkham/components/LocationMapCanvas.vue) hiện `nameOf(l.cardCode)` cả khi phòng chưa reveal. Với các location có tên mặt sau khác mặt trước, tên mặt trước bị lộ.

## Mục tiêu

Mỗi địa điểm có một **bảng tên** dễ đọc trên mọi nền. Người chơi chỉ cần liếc qua là thấy trạng thái chính. Ngoài bảng tên còn có một **thẻ thông tin** để xem chi tiết và bấm action. Bảng tên và thẻ thông tin dùng chung cho cả ba kiểu vẽ: cutaway, map dạng node và mini-map.

```
 Bảng tên (đã reveal)                Bảng tên (chưa reveal)
 ┌──────────────────────────┐        ┌ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┐
 │ ● Attic            ⚠  ⓘ │        │ ? Parlor          ⓘ │
 │ 🔍2   ◇1/2   ☠1   👤 ⚔2 │        │ Chưa lật · vào để lật │
 └──────────────────────────┘        └ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┘
  ● bạn ở đây / ↔ kề bên đi được / ✦ đang được chọn làm mục tiêu
```

## Phases

| # | Giai đoạn | Trạng thái |
|---|---|---|
| 1 | [Dữ liệu địa điểm và chặn spoiler](./phase-01-location-info-model.md) | Code xong, chưa thử trên app |
| 2 | [Bảng tên địa điểm cho 3 kiểu vẽ](./phase-02-location-marker-plate.md) | Code xong, chưa thử trên app |
| 3 | [Thẻ thông tin và action nhanh](./phase-03-location-info-card.md) | Code xong, chưa thử trên app |
| 4 | [Chú giải, mobile, kiểm tra](./phase-04-legend-mobile-verify.md) | Code xong, chưa thử trên app |

Thứ tự phụ thuộc: phase 2 và 3 đều cần model của phase 1. Phase 2 và 3 sửa các file khác nhau nên làm song song được. Phase 4 làm sau cùng.

Tham chiếu: [Port bản đồ The Gathering](../261106-0910-gathering-map-port/plan.md), [Ảnh địa điểm Hemlock Vale](../261106-1309-hemlock-vale-location-art/plan.md). Kế hoạch Hemlock Vale sẽ thêm cutaway mới, nên bảng tên không được gắn với tọa độ riêng của nhà The Gathering.

## Tiêu chí hoàn thành

- [ ] Mỗi location trong cả 3 kiểu vẽ có bảng tên nền tối, viền rõ, chữ đọc được trên mọi tranh nền. Hai trạng thái reveal và chưa reveal khác nhau rõ, dùng cả icon lẫn kiểu viền, không chỉ dựa vào màu.
- [ ] Location đã reveal hiện shroud hiện tại, clue còn lại trên số clue in, doom, số điều tra viên và số kẻ địch. Location chưa reveal chỉ hiện những gì mặt sau thẻ cho phép.
- [ ] Không bao giờ lộ tên mặt trước, text, shroud, clue in, victory hay hiệu ứng "Forced – after you enter" của location chưa reveal.
- [ ] Bảng tên có dấu hiệu "có hiệu ứng khi vào" (⚠) nếu text mặt đang lộ có Forced hoặc after you enter.
- [ ] Thẻ thông tin hiện ảnh thẻ, tên, traits, text đã format icon, flavor, shroud, clue, victory, các nơi kết nối, ai đang ở đó, thẻ gắn vào phòng, và các action server đang cho phép.
- [ ] Move và Investigate bấm được ngay từ thẻ thông tin hoặc bảng tên. Nút gửi đúng index choice server đưa, không tự tạo luồng di chuyển riêng.
- [ ] Xem thông tin không bao giờ tự gửi choice. Chọn target vẫn là một thao tác riêng, rõ ràng.
- [ ] Khi card DB chưa tải xong, thẻ thông tin vẫn hiện tên và token, không lỗi.
- [ ] Chạy được trên desktop, mobile ≤ 800px, khi reload, ván 1 và 4 điều tra viên. `vue-tsc` và test unit đều qua.

## Câu hỏi còn mở

1. **Xem hay chọn**: đề xuất dùng hover hoặc nút ⓘ để xem, click để chọn hoặc mở thẻ. Trên mobile, chạm lần đầu mở thẻ thông tin và trong thẻ có nút "Chọn nơi này". Có đồng ý không?
2. **Shroud bị modifier**: server chỉ gửi shroud in trên thẻ. Đề xuất decode `ShroudModifier` và `SetShroud` ở frontend rồi tự tính. Cách còn lại là cho server gửi sẵn giá trị đã tính, đúng hơn nhưng phải sửa backend.
3. **Thẻ thông tin và LocationActionPanel**: đề xuất thẻ thông tin là popover nổi cạnh bảng tên, panel Actions bên phải giữ nguyên. Hoặc gộp thẻ thông tin làm phần đầu của panel?
4. **Tùy chọn spoiler**: có muốn thêm setting "hiện text mặt trước của location chưa lật" (mặc định tắt) cho người đã thuộc kịch bản không?
