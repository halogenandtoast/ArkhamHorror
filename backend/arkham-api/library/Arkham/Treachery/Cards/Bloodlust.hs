module Arkham.Treachery.Cards.Bloodlust (bloodlust) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Bloodlust = Bloodlust TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodlust :: TreacheryCard Bloodlust
bloodlust = treachery Bloodlust Cards.bloodlust

instance HasAbilities Bloodlust where
  getAbilities (Bloodlust attrs) =
    [ limited (MaxPer Cards.bloodlust PerTestOrAbility 1)
        $ restricted
          attrs
          1
          ( DuringSkillTest
              $ YourSkillTest
              $ WhileAttackingAnEnemy AnyEnemy
              <> SkillTestSourceMatches (SourceIsAsset $ assetIs Assets.theHungeringBlade1 <> AssetControlledBy You)
          )
        $ FastAbility (ShuffleIntoDeckCost $ toTarget attrs)
    ]

instance RunMessage Bloodlust where
  runMessage msg t@(Bloodlust attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mTheHungeringBlade <-
        selectOne
          $ assetIs Assets.theHungeringBlade1
          <> assetControlledBy iid
          <> AssetWithTokens (atLeast 2) Offering

      case mTheHungeringBlade of
        Nothing -> do
          assignHorror iid attrs 1
          shuffleIntoDeck iid attrs
        Just theHungeringBlade -> do
          removeTokens attrs theHungeringBlade Offering 2
          attachTreachery attrs theHungeringBlade

      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      pure t
    _ -> Bloodlust <$> liftRunMessage msg attrs
