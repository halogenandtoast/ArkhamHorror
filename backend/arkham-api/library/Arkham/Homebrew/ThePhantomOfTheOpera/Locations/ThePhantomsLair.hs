module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.ThePhantomsLair (thePhantomsLair) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveAllTo)
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype ThePhantomsLair = ThePhantomsLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePhantomsLair :: LocationCard ThePhantomsLair
thePhantomsLair = symbolLabel $ location ThePhantomsLair Cards.thePhantomsLair 2 (PerPlayer 1)

exposedThisRound :: ModifierType
exposedThisRound = CampaignModifier "thePhantomOfTheOpera.exposedThisRound"

instance HasAbilities ThePhantomsLair where
  getAbilities (ThePhantomsLair a) =
    if a.revealed
      then
        extendRevealed
          a
          [ scenarioI18n
              $ withI18nTooltip "thePhantomsLair.despair"
              $ restricted
                a
                1
                ( Here
                    <> youExist (InvestigatorWithoutModifier exposedThisRound)
                    <> not_ (HasHistory #round You (CluesDiscovered $ atLeast 1))
                )
              $ forced
              $ TurnEnds #after You
          , scenarioI18n
              $ withI18nTooltip "thePhantomsLair.lure"
              $ restricted a 2 Here actionAbility
          , tracker a
          ]
      else
        extendUnrevealed
          a
          [ restricted a 3 (ActExists (ActWithClues (AtLeast $ PerPlayer 1)))
              $ Objective
              $ triggered (RoundEnds #when) Free
          , tracker a
          ]
   where
    tracker x =
      mkAbility x 4
        $ silent
        $ mapOneOf (CampaignEvent #after (Just Anyone)) ["exposed[enemy]", "exposed[decoy]"]

getExposedBy :: [Window] -> Maybe InvestigatorId
getExposedBy = \case
  (windowType -> Window.CampaignEvent _ mWho _) : _ -> mWho
  _ : rest -> getExposedBy rest
  [] -> Nothing

instance RunMessage ThePhantomsLair where
  runMessage msg l@(ThePhantomsLair attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      placeTokens (attrs.ability 2) attrs #clue 1
      doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 2) -> do
      required <- perPlayer 2
      when (attrs.clues >= required && attrs.token #horror == 0)
        $ placeTokens (attrs.ability 2) attrs #horror 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      moveAllTo (attrs.ability 3) attrs.id
      advanceCurrentAct (attrs.ability 3)
      pure l
    UseCardAbility _ (isSource attrs -> True) 4 (getExposedBy -> Just iid) _ -> do
      roundModifier (attrs.ability 4) iid exposedThisRound
      pure l
    _ -> ThePhantomsLair <$> liftRunMessage msg attrs
