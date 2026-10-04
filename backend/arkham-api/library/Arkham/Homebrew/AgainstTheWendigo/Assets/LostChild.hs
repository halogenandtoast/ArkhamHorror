module Arkham.Homebrew.AgainstTheWendigo.Assets.LostChild (lostChild) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Civilized, pattern Sarcee)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype LostChild = LostChild AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostChild :: AssetCard LostChild
lostChild = allyWith LostChild Cards.lostChild (1, 1) noSlots

instance HasAbilities LostChild where
  getAbilities (LostChild a) =
    [ -- "If an agenda or an act advances, or if Lost Child is removed from the
      -- game, each investigator takes 1 direct horror."
      mkAbility a 1 $ forced $ oneOf [AgendaAdvances #when AnyAgenda, ActAdvances #when AnyAct]
    , -- "{action}: Parley. Take control of Lost Child."
      restricted a 2 (OnSameLocation <> not_ ControlsThis) parleyAction_
    , -- "{reaction} If you are in a Sarcee location: Put Lost Child in the
      -- victory display."
      controlled a 3 (exists $ YourLocation <> LocationWithTrait Sarcee)
        $ triggered (TurnBegins #after You) mempty
    ]

instance RunMessage LostChild where
  runMessage msg a@(LostChild attrs) = runQueueT $ case msg of
    -- "If you are on a Civilized location, Lost Child gains Surge then discard
    -- her. Otherwise, attach this card to your location."
    Revelation iid (isSource attrs -> True) -> do
      civilized <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Civilized
      if civilized
        then do
          push $ GainSurge (toSource attrs) (toTarget attrs)
          toDiscard attrs attrs
        else withLocationOf iid \lid -> place attrs (AttachedToLocation lid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> directHorror iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      takeControlOfAsset iid attrs.id
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      addToVictory iid attrs
      pure a
    _ -> LostChild <$> liftRunMessage msg attrs
