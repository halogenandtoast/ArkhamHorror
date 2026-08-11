module Arkham.Homebrew.DarkMatter.Assets.ErwinSimmonsQuantumPhysicist (
  erwinSimmonsQuantumPhysicist,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  campaignI18n,
  drawAllFacedownCards,
  placeFacedownInThreatArea,
 )
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ErwinSimmonsQuantumPhysicist = ErwinSimmonsQuantumPhysicist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erwinSimmonsQuantumPhysicist :: AssetCard ErwinSimmonsQuantumPhysicist
erwinSimmonsQuantumPhysicist =
  asset ErwinSimmonsQuantumPhysicist Cards.erwinSimmonsQuantumPhysicist

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
    [ controlled_ a 1 $ freeReaction $ TurnBegins #when You
    , controlled_ a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage ErwinSimmonsQuantumPhysicist where
  runMessage msg a@(ErwinSimmonsQuantumPhysicist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeFacedownInThreatArea iid 1
      sid <- getRandom
      canFight <- selectAny $ CanFightEnemy (toSource $ attrs.ability 1)
      canEvade <- selectAny $ enemyCanBeEvadedBy (attrs.ability 1)
      chooseOneM iid $ campaignI18n do
        when canFight
          $ labeled' "erwinSimmons.fight"
          $ chooseFightEnemy sid iid (attrs.ability 1)
        when canEvade
          $ labeled' "erwinSimmons.evade"
          $ chooseEvadeEnemy sid iid (attrs.ability 1)
        labeled' "erwinSimmons.investigate" $ pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure a
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      dealAssetDamage attrs.id (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawAllFacedownCards iid
      pure a
    _ -> ErwinSimmonsQuantumPhysicist <$> liftRunMessage msg attrs
