module Arkham.Asset.Cards.EnchantedArmor2 (enchantedArmor2, EnchantedArmor2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), pattern PlacedDamage, pattern PlacedHorror)

newtype EnchantedArmor2 = EnchantedArmor2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedArmor2 :: AssetCard EnchantedArmor2
enchantedArmor2 = assetWith EnchantedArmor2 Cards.enchantedArmor2 (setMeta @(Int, Int) (0, 0))

instance HasModifiersFor EnchantedArmor2 where
  getModifiersFor (InvestigatorTarget iid) (EnchantedArmor2 attrs) =
    case attrs.controller of
      Just controller | controller == iid -> do
        pure $ toModifiers attrs [CanAssignDamageToAsset attrs.id, CanAssignHorrorToAsset attrs.id]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities EnchantedArmor2 where
  getAbilities (EnchantedArmor2 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ forced
        $ oneOf
          [PlacedCounterOnAsset #after (be attrs) AnySource cType $ atLeast 1 | cType <- [#horror, #damage]]
    ]

-- We should not need the timing, but the when and after windows for placing tokens occur at the same time
getTotal :: AssetAttrs -> [Window] -> (Int, Int)
getTotal attrs xs = go (0, 0) xs
 where
  go acc [] = acc
  go acc@(x, y) (w : ws) = case (windowTiming w, windowType w) of
    (Timing.After, PlacedDamage _ (isTarget attrs -> True) n) -> go (x + n, y) ws
    (Timing.After, PlacedHorror _ (isTarget attrs -> True) n) -> go (x, y + n) ws
    _ -> go acc ws

instance RunMessage EnchantedArmor2 where
  runMessage msg a@(EnchantedArmor2 attrs) = case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      player <- getPlayer iid
      iids <- select $ affectsOthers $ colocatedWith iid
      push $ chooseOrRunOne player $ targetLabels iids $ only . (`TakeControlOfAsset` (toId attrs))
      EnchantedArmor2 <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 (getTotal attrs -> (x, y)) _ -> do
      for_ attrs.owner \owner -> do
        stillAlive <- selectAny $ InvestigatorWithId owner
        pushWhen stillAlive
          $ beginSkillTest owner (attrs.ability 1) owner #willpower (attrs.damage + attrs.horror)
      pure $ EnchantedArmor2 $ attrs & setMeta (x, y)
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let (x, y) = toResult @(Int, Int) attrs.meta
      pushAll [toDiscardBy iid (attrs.ability 1) attrs, assignDamageAndHorror iid (attrs.ability 1) x y]
      pure a
    _ -> EnchantedArmor2 <$> runMessage msg attrs
