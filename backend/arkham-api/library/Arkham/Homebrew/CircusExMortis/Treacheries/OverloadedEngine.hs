module Arkham.Homebrew.CircusExMortis.Treacheries.OverloadedEngine (overloadedEngine) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype OverloadedEngine = OverloadedEngine TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overloadedEngine :: TreacheryCard OverloadedEngine
overloadedEngine = treachery OverloadedEngine Cards.overloadedEngine

instance HasAbilities OverloadedEngine where
  getAbilities (OverloadedEngine attrs) =
    [ mkAbility attrs 1 $ forced $ RoundEnds #when
    , skillTestAbility $ restricted attrs 2 OnSameLocation actionAbility
    ]

instance RunMessage OverloadedEngine where
  runMessage msg t@(OverloadedEngine attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectOne (locationIs Locations.locomotiveEngine) >>= traverse_ (attachTreachery attrs)
      selectOne (assetIs Assets.ralphDykstra) >>= traverse_ (exhaustEnemy attrs)
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      withMatch (locationIs Locations.locomotiveEngine) \loc -> placeDoom attrs loc 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 2) attrs sType (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid attrs attrs
      pure t
    _ -> OverloadedEngine <$> liftRunMessage msg attrs
