module Arkham.Location.Cards.DiningRoom (
  diningRoom,
  DiningRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy

newtype DiningRoom = DiningRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningRoom :: LocationCard DiningRoom
diningRoom = location DiningRoom Cards.diningRoom 2 (Static 0)

instance HasAbilities DiningRoom where
  getAbilities (DiningRoom attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility
        attrs
        1
        ( Here
            <> InvestigatorExists
              (HealableInvestigator (toSource attrs) HorrorType You)
        )
        $ ActionAbility []
        $ ActionCost 1
      | locationRevealed attrs
      ]

instance RunMessage DiningRoom where
  runMessage msg l@(DiningRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      pushAll
        $ [HealHorror (toTarget iid) (attrs.ability 1) 1 | canHeal]
        <> [RequestChaosTokens source (Just iid) (Reveal 1) SetAside]
      pure l
    RequestedChaosTokens source (Just iid) tokens | isSource attrs source -> do
      push $ ResetChaosTokens (toSource attrs)
      chaosTokenFaces <- getModifiedChaosTokenFaces tokens
      pushAll
        $ concatMap
          ( \case
              chaosTokenFace
                | chaosTokenFace `elem` [Skull, AutoFail] ->
                    [ InvestigatorAssignDamage iid source DamageAny 0 1
                    , PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
                    ]
              _ -> []
          )
          chaosTokenFaces
      pure l
    _ -> DiningRoom <$> runMessage msg attrs
