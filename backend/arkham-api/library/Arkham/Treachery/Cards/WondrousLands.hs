module Arkham.Treachery.Cards.WondrousLands (wondrousLands) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WondrousLands = WondrousLands TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wondrousLands :: TreacheryCard WondrousLands
wondrousLands = treachery WondrousLands Cards.wondrousLands

instance HasModifiersFor WondrousLands where
  getModifiersFor (WondrousLands attrs) = case attrs.placement of
    AttachedToLocation lid -> modified_ attrs lid [ShroudModifier (-2)]
    _ -> pure mempty

instance HasAbilities WondrousLands where
  getAbilities (WondrousLands a) = case a.attached.location of
    Just lid ->
      [ forcedAbility a 1
          $ SkillTestResult #after You (WhileInvestigating $ LocationWithId lid) (SuccessResult AnyValue)
      ]
    _ -> []

instance RunMessage WondrousLands where
  runMessage msg t@(WondrousLands attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      field InvestigatorLocation iid >>= \case
        Nothing -> gainSurge attrs
        Just loc -> do
          hasClues <- fieldMap LocationClues (> 0) loc
          if hasClues then attachTreachery attrs loc else gainSurge attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      toDiscardBy iid (attrs.ability 1) attrs
      placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> WondrousLands <$> liftRunMessage msg attrs
