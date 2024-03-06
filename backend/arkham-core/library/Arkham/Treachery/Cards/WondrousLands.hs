module Arkham.Treachery.Cards.WondrousLands (wondrousLands, WondrousLands (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WondrousLands = WondrousLands TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wondrousLands :: TreacheryCard WondrousLands
wondrousLands = treachery WondrousLands Cards.wondrousLands

instance HasModifiersFor WondrousLands where
  getModifiersFor (LocationTarget lid) (WondrousLands attrs) =
    pure $ toModifiers attrs [ShroudModifier (-2) | treacheryOnLocation lid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities WondrousLands where
  getAbilities (WondrousLands a) = case treacheryAttachedTarget a of
    Just (LocationTarget lid) ->
      [ forcedAbility a 1
          $ SkillTestResult #after You (WhileInvestigating $ LocationWithId lid) (SuccessResult AnyValue)
      ]
    _ -> []

instance RunMessage WondrousLands where
  runMessage msg t@(WondrousLands attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mLocation <- field InvestigatorLocation iid
      case mLocation of
        Nothing -> push $ gainSurge attrs
        Just loc -> do
          hasClues <- fieldMap LocationClues (> 0) loc
          push $ if hasClues then attachTreachery attrs loc else gainSurge attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ assignHorror iid (attrs.ability 1) 1
        , toDiscardBy iid (attrs.ability 1) attrs
        , placeDoomOnAgendaAndCheckAdvance
        ]
      pure t
    _ -> WondrousLands <$> runMessage msg attrs
