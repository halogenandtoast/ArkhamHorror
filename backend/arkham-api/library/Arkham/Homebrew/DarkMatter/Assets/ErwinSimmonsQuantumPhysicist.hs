module Arkham.Homebrew.DarkMatter.Assets.ErwinSimmonsQuantumPhysicist (
  erwinSimmonsQuantumPhysicist,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (DoNotTakeUpSlot), modifySelfWhen)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  canPlaceFacedownInThreatArea,
  drawAllFacedownCards,
  placeFacedownInThreatArea,
 )
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ErwinSimmonsQuantumPhysicist = ErwinSimmonsQuantumPhysicist AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erwinSimmonsQuantumPhysicist :: AssetCard ErwinSimmonsQuantumPhysicist
erwinSimmonsQuantumPhysicist =
  ally ErwinSimmonsQuantumPhysicist Cards.erwinSimmonsQuantumPhysicist (3, 1)

-- Elbrus Station 1b: "Put the set aside Erwin Simmons (Quantum Physicist) story
-- asset into play under any investigator's control. This story asset does not
-- take up an ally slot for this scenario." The ownerless check scopes it to that
-- scenario copy; the copy earned at Resolution 3 is deck-owned.
instance HasModifiersFor ErwinSimmonsQuantumPhysicist where
  getModifiersFor (ErwinSimmonsQuantumPhysicist a) =
    modifySelfWhen a (isNothing a.owner) [DoNotTakeUpSlot #ally]

{- | "[reaction] When your turn begins, put the top card of the encounter deck
face-down into your threat area: Fight/Evade/Investigate. If you fail, deal 1
damage to Erwin Simmons.
Forced - When Erwin Simmons leaves play: Draw all face-down encounter cards in
your threat area."

TODO(homebrew): the face-down placement is the printed *cost* of the reaction;
it is modelled as the first thing the ability does, since "place the top card of
the encounter deck face down" is not expressible as a 'Cost'.
-}
instance HasAbilities ErwinSimmonsQuantumPhysicist where
  getAbilities (ErwinSimmonsQuantumPhysicist a) =
    [ restricted a 1 (ControlsThis <> canPlaceFacedownInThreatArea)
        $ freeReaction
        $ TurnBegins #when You
    , controlled_ a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage ErwinSimmonsQuantumPhysicist where
  runMessage msg a@(ErwinSimmonsQuantumPhysicist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeFacedownInThreatArea iid 1
      sid <- getRandom
      canFight <- selectAny $ CanFightEnemy (toSource $ attrs.ability 1)
      canEvade <- selectAny $ enemyCanBeEvadedBy (attrs.ability 1)
      chooseOneM iid $ withI18n do
        when canFight $ labeled "fight" $ chooseFightEnemy sid iid (attrs.ability 1)
        when canEvade $ labeled "evade" $ chooseEvadeEnemy sid iid (attrs.ability 1)
        labeled "investigate" $ investigate sid iid (attrs.ability 1)
      pure a
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      dealAssetDamage attrs.id (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawAllFacedownCards iid
      pure a
    _ -> ErwinSimmonsQuantumPhysicist <$> liftRunMessage msg attrs
