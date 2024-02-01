module Arkham.Treachery.Cards.LowOnSupplies (
  lowOnSupplies,
  LowOnSupplies (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LowOnSupplies = LowOnSupplies TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

lowOnSupplies :: TreacheryCard LowOnSupplies
lowOnSupplies = treachery LowOnSupplies Cards.lowOnSupplies

instance RunMessage LowOnSupplies where
  runMessage msg t@(LowOnSupplies attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      anyWithResources <- selectAny InvestigatorWithAnyResources
      hasAssets <- selectAny (HasMatchingAsset AnyAsset)
      investigatorIds <- getInvestigatorIds
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ ( if anyWithResources
              then
                [ Label
                    "Each investigator loses 2 resources."
                    [LoseResources iid' (toSource attrs) 2 | iid' <- investigatorIds]
                ]
              else []
          )
        <> [ Label
              "Each investigator takes 1 damage."
              [ InvestigatorAssignDamage iid' source DamageAny 1 0
              | iid' <- investigatorIds
              ]
           ]
        <> ( if hasAssets
              then
                [ Label
                    "Each investigator chooses and discards an asset he or she controls."
                    [ ChooseAndDiscardAsset iid' (toSource attrs) AnyAsset
                    | iid' <- investigatorIds
                    ]
                ]
              else []
           )
      pure t
    _ -> LowOnSupplies <$> runMessage msg attrs
