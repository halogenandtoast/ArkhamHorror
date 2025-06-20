module Arkham.Location.Cards.LanternChamber (lanternChamber) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Name (toTitle)
import Data.Function (on)
import Data.List.Extra (nubOrdBy)

newtype LanternChamber = LanternChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lanternChamber :: LocationCard LanternChamber
lanternChamber =
  locationWith LanternChamber Cards.lanternChamber 5 (PerPlayer 2)
    $ costToEnterUnrevealedL
    .~ HorrorCost ThisCard YouTarget 1

instance HasAbilities LanternChamber where
  getAbilities (LanternChamber a) =
    extendRevealed
      a
      [ playerLimit PerTurn $ restricted a 1 (Here <> DuringTurn You) $ FastAbility Free
      , restricted
          a
          2
          ( Here
              <> thisExists a LocationWithoutClues
              <> oneOf
                [ exists (IgnoreVisibility $ AssetWithModifier $ ScenarioModifier "spellbound")
                , exists (EnemyWithModifier $ ScenarioModifier "spellbound")
                ]
          )
          actionAbility
      ]

instance RunMessage LanternChamber where
  runMessage msg l@(LanternChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <-
        sortOn toTitle
          . nubOrdBy (compare `on` toTitle)
          <$> findAllCards (`cardMatch` oneOf [cardIs Assets.jewelOfSarnath, #enemy, #treachery])
      chooseOneDropDown iid [(card.title, ForTarget (toTarget card) msg) | card <- cards]
      discoverAt NotInvestigate iid (attrs.ability 1) attrs 1
      pure l
    ForTarget (CardIdTarget cid) (UseThisAbility iid (isSource attrs -> True) 1) -> do
      card <- getCard cid
      deck <- unDeck <$> getEncounterDeck
      case deck of
        (x : _) -> do
          focusCards [x] (continue_ iid)
          if x.title == card.title
            then addToDiscard iid (only x)
            else drawCard iid x
        _ -> pure ()
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      assets <- select $ IgnoreVisibility $ AssetWithModifier (ScenarioModifier "spellbound")
      enemies <- select $ EnemyWithModifier (ScenarioModifier "spellbound")
      chooseOneM iid do
        targets assets $ flipOverBy iid (attrs.ability 2)
        targets enemies $ flipOverBy iid (attrs.ability 2)
      pure l
    _ -> LanternChamber <$> liftRunMessage msg attrs
