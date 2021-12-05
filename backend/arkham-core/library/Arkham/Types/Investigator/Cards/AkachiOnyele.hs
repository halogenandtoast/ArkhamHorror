module Arkham.Types.Investigator.Cards.AkachiOnyele where

import Arkham.Prelude

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Types.Asset.Uses
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target

newtype AkachiOnyele = AkachiOnyele InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasCount StartingUsesCount env (AssetId, UseType) => HasModifiersFor env AkachiOnyele where
  getModifiersFor (InvestigatorSource iid) (AssetTarget aid) (AkachiOnyele attrs)
    | toId attrs == iid =
      do
        startingChargesCount <- unStartingUsesCount <$> getCount (aid, Charge)
        pure $
          toModifiers
            attrs
            [AdditionalStartingUses 1 | startingChargesCount > 0]
  getModifiersFor _ _ _ = pure []

akachiOnyele :: InvestigatorCard AkachiOnyele
akachiOnyele =
  investigator
    AkachiOnyele
    Cards.akachiOnyele
    Stats
      { health = 6
      , sanity = 8
      , willpower = 5
      , intellect = 2
      , combat = 3
      , agility = 3
      }

instance HasTokenValue env AkachiOnyele where
  getTokenValue (AkachiOnyele attrs) iid ElderSign
    | iid == investigatorId attrs =
      pure $
        TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance InvestigatorRunner env => RunMessage env AkachiOnyele where
  runMessage msg i@(AkachiOnyele attrs) = case msg of
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      targets <-
        map AssetTarget
          <$> filterM
            (fmap ((> 0) . unUsesCount) . getCount . (,Charge))
            (setToList $ investigatorAssets attrs)
      i
        <$ when
          (notNull targets)
          ( push $
              chooseOne
                iid
                ( Done "Do not use Elder Sign ability" :
                    [ TargetLabel target [AddUses target Charge 1]
                    | target <- targets
                    ]
                )
          )
    _ -> AkachiOnyele <$> runMessage msg attrs
