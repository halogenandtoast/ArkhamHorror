module Arkham.Location.Cards.DiningRoom (
  diningRoom,
  DiningRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.RequestedTokenStrategy
import Arkham.Token

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
        $ ActionAbility Nothing
        $ ActionCost 1
      | locationRevealed attrs
      ]

instance RunMessage DiningRoom where
  runMessage msg l@(DiningRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mHealHorror <- getHealHorrorMessage attrs 1 iid
      pushAll $
        maybeToList mHealHorror
          <> [RequestTokens source (Just iid) (Reveal 1) SetAside]
      pure l
    RequestedTokens source (Just iid) tokens | isSource attrs source -> do
      push $ ResetTokens (toSource attrs)
      tokenFaces <- getModifiedTokenFaces tokens
      let
        msgs =
          concatMap
            ( \case
                tokenFace
                  | tokenFace `elem` [Skull, AutoFail] ->
                      [ InvestigatorAssignDamage iid source DamageAny 0 1
                      , PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
                      ]
                _ -> []
            )
            tokenFaces
      l <$ pushAll msgs
    _ -> DiningRoom <$> runMessage msg attrs
