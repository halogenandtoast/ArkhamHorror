module Arkham.Asset.Assets.DirectiveSeekTheTruth (directiveSeekTheTruth) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (DiscoverClues)
import Arkham.Capability
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.History
import Arkham.Matcher
import Data.Aeson.Lens (_Bool)

newtype DirectiveSeekTheTruth = DirectiveSeekTheTruth AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

directiveSeekTheTruth :: AssetCard DirectiveSeekTheTruth
directiveSeekTheTruth = asset DirectiveSeekTheTruth Cards.directiveSeekTheTruth

instance HasModifiersFor DirectiveSeekTheTruth where
  getModifiersFor (DirectiveSeekTheTruth a) = for_ a.controller \iid ->
    unless a.flipped do
      discoveredClue <- notNull <$> getHistoryField RoundHistory iid HistoryCluesDiscovered
      valid <- selectAny $ locationWithInvestigator iid <> LocationWithAnyClues
      when valid do
        let ignored = fromMaybe False $ a ^? metaMapL . ix "ignore_regulation" . _Bool
        modifiedWhen_
          a
          (not discoveredClue && not ignored)
          iid
          [CannotCommitCards AnyCard]

instance HasAbilities DirectiveSeekTheTruth where
  getAbilities (DirectiveSeekTheTruth a) =
    [ controlled a 1 (exists (EnemyAt YourLocation) <> can.draw.cards You)
        $ triggered (DiscoverClues #after You Anywhere AnyValue) (exhaust a)
    | not a.flipped
    ]

instance RunMessage DirectiveSeekTheTruth where
  runMessage msg a@(DirectiveSeekTheTruth attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      pure . DirectiveSeekTheTruth $ attrs & flippedL .~ True
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> DirectiveSeekTheTruth <$> liftRunMessage msg attrs
