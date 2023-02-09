module Arkham.Treachery.Cards.Snakescourge
  ( snakescourge
  , Snakescourge(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types ( Field (..) )
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait ( Trait (Item) )
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Snakescourge = Snakescourge TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snakescourge :: TreacheryCard Snakescourge
snakescourge = treachery Snakescourge Cards.snakescourge

instance HasModifiersFor Snakescourge where
  getModifiersFor (AssetTarget aid) (Snakescourge attrs) = do
    miid <- field AssetController aid
    nonweaknessItem <- aid <=~> (NonWeaknessAsset <> AssetWithTrait Item)
    pure $ toModifiers
      attrs
      [ Blank
      | iid <- maybeToList miid
      , nonweaknessItem
      , treacheryOnInvestigator iid attrs
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities Snakescourge where
  getAbilities (Snakescourge a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ RoundEnds
        Timing.When
    ]

instance RunMessage Snakescourge where
  runMessage msg t@(Snakescourge attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      isPoisoned <- getIsPoisoned iid
      pushAll
        $ AttachTreachery (toId t) (InvestigatorTarget iid)
        : [ Surge iid source | isPoisoned ]
      pure t
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      t <$ push (Discard (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> Snakescourge <$> runMessage msg attrs
