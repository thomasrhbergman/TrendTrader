unit Global.Resources;

interface

resourcestring
  rcNameNotUnique               = 'Name is not unique! ' + sLineBreak;
  rcQuestionContinueEditing     = 'Do you want to continue editing?';
  rcRequiredValue               = 'Required value for "%s"' + sLineBreak;
  rsAllNodesDeletedConfirmation = 'All nodes be deleted.' + sLineBreak + ' Continue?';
  rsChangingDocument            = 'Changing the Document affects overall template in Template Creator';
  rsCheckInstruments             = 'Run now check instruments in background?';
  rsColumnDeletedConfirmation   = 'Column "%s" be deleted. Continue?';
  rsColumnExists                = 'Column "%s" already exists!';
  rsNotSupportsIMonitor         = 'Monitor not supports IMonitor';
  rsSuccessful                  = 'The operation was successful';
  rsUpdatePrice                  = 'Run now update latest price in background?';
  rsPrecautionaryQuantity        = 'Quantity is greater than allowed in Precautionary Settings (%d).' + sLineBreak +
                                   'Do you want to change the Quantity?';

  rsInstrumentValidationError    = 'There is a instrument validation error:' + sLineBreak + '%s !';
  rsPrecautionaryMaxAllowedPrice = 'The price is greater than allowed MaxAllowedPrice (%f)!';
  rsPrecautionaryMinAllowedPrice = 'The price is below than allowed MaxAllowedPrice (%f)!';

  rsOrderCancelCancelled = 'The order is already cancelled';
  rsOrderCancelFilled    = 'Please cancel the order in a Broker system';
  rsOrderCancellation    = 'Order cancellation sent to Broker!';
  rsOrderCancelSleeping  = 'The order is not active yet';
  rsTypeOperationNotSelected = 'Type Operation not selected!';


implementation

end.
