module Arkham.Types.Treachery.Cards.AngeredSpirits
  ( angeredSpirits
  , AngeredSpirits(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Uses
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (FastPlayerWindow)
import Arkham.Types.Message hiding (InvestigatorEliminated)
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AngeredSpirits = AngeredSpirits TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

angeredSpirits :: TreacheryCard AngeredSpirits
angeredSpirits =
  treacheryWith AngeredSpirits Cards.angeredSpirits (resourcesL ?~ 0)

instance HasAbilities AngeredSpirits where
  getAbilities (AngeredSpirits a) =
    restrictedAbility
        a
        1
        OnSameLocation
        (FastAbility
        $ ExhaustAssetCost
        $ AssetWithTrait Spell
        <> AssetOwnedBy You
        )
      : [ restrictedAbility a 2 (ChargesOnThis $ EqualTo $ Static 0)
          $ ForcedAbility
          $ OrWindowMatcher
              [ GameEnds Timing.When
              , InvestigatorEliminated Timing.When (InvestigatorWithId iid)
              ]
        | iid <- maybeToList (treacheryOwner a)
        ]

instance TreacheryRunner env => RunMessage env AngeredSpirits where
  runMessage msg t@(AngeredSpirits attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ RemoveCardFromHand iid (toCardId attrs)
      , AttachTreachery (toId attrs) (InvestigatorTarget iid)
      ]
    UseCardAbility _ source _ 1 (ExhaustPayment [target])
      | isSource attrs source
      -> t <$ pushAll
        [SpendUses target Charge 1, PlaceResources (toTarget attrs) 1]
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      withTreacheryInvestigator attrs
        $ \tormented -> t <$ push (SufferTrauma tormented 1 0)
    _ -> AngeredSpirits <$> runMessage msg attrs
