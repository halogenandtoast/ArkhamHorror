module Arkham.Treachery.Cards.Bloodlust (
  bloodlust,
  Bloodlust (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Deck
import Arkham.Matcher
import Arkham.Message
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Bloodlust = Bloodlust TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

bloodlust :: TreacheryCard Bloodlust
bloodlust = treachery Bloodlust Cards.bloodlust

instance HasAbilities Bloodlust where
  getAbilities (Bloodlust attrs) =
    [ limitedAbility (MaxPer Cards.bloodlust PerTestOrAbility 1)
        $ restrictedAbility
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
  runMessage msg t@(Bloodlust attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mTheHungeringBlade <-
        selectOne
          $ assetIs Assets.theHungeringBlade1
          <> assetControlledBy iid
          <> AssetWithTokens (atLeast 2) Offering

      case mTheHungeringBlade of
        Nothing ->
          pushAll
            [assignHorror iid (toSource attrs) 1, ShuffleIntoDeck (InvestigatorDeck iid) (toTarget attrs)]
        Just theHungeringBlade ->
          pushAll
            [ RemoveTokens (toSource attrs) (toTarget theHungeringBlade) Offering 2
            , attachTreachery attrs theHungeringBlade
            ]

      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ skillTestModifier (toAbilitySource attrs 1) iid (DamageDealt 1)
      pure t
    _ -> Bloodlust <$> runMessage msg attrs
