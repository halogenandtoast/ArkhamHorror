module Arkham.Location.Cards.Valusia (
  valusia,
  Valusia (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (DiscoverClues)

newtype Valusia = Valusia LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

valusia :: LocationCard Valusia
valusia = location Valusia Cards.valusia 4 (Static 2)

instance HasAbilities Valusia where
  getAbilities (Valusia a) =
    withBaseAbilities
      a
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility
            a
            1
            ( Here
                <> ChaosTokenCountIs
                  (IncludeSealed $ ChaosTokenFaceIs Cultist)
                  (AtLeast $ Static 3)
            )
          $ ActionAbility []
          $ ActionCost 1
      ]

instance RunMessage Valusia where
  runMessage msg (Valusia attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mHealHorror <- getHealHorrorMessage attrs 2 iid
      canHealDamage <- canHaveDamageHealed attrs iid
      pushAll
        $ [ HealDamage (InvestigatorTarget iid) (toSource attrs) 2
          | canHealDamage
          ]
        <> maybeToList mHealHorror
      pure $ Valusia $ attrs & shroudL .~ 0
    _ -> Valusia <$> runMessage msg attrs
